/****************************************************************************
 Copyright (c) 2020-2021 Xiamen Yaji Software Co., Ltd.

 http://www.cocos.com

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated engine source code (the "Software"), a limited,
 worldwide, royalty-free, non-assignable, revocable and non-exclusive license
 to use Cocos Creator solely to develop games on your target platforms. You shall
 not use Cocos Creator software for developing other software or tools that's
 used for developing games. You are not granted to publish, distribute,
 sublicense, and/or sell copies of Cocos Creator.

 The software or tools in this License Agreement are licensed, not sold.
 Xiamen Yaji Software Co., Ltd. reserves all rights not expressly granted to you.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
****************************************************************************/

#include "ScriptEngine.h"

#if SCRIPT_ENGINE_TYPE == SCRIPT_ENGINE_V8

    #include "../MappingUtils.h"
    #include "../State.h"
    #include "Class.h"
    #include "Object.h"
    #include "Utils.h"
    #include "platform/FileUtils.h"

    #include <array>
    #include <sstream>

    #if SE_ENABLE_INSPECTOR
        #include "debugger/env.h"
        #include "debugger/inspector_agent.h"
        #include "debugger/node.h"

    #endif

    #include <sstream>

    #define EXPOSE_GC "__jsb_gc__"

const unsigned int JSB_STACK_FRAME_LIMIT = 20;

    #ifdef CC_DEBUG
unsigned int                    jsbInvocationCount = 0;
std::map<std::string, unsigned> jsbFunctionInvokedRecords;
    #endif

    #define RETRUN_VAL_IF_FAIL(cond, val) \
        if (!(cond)) return val

namespace se {

Class *__jsb_CCPrivateData_class = nullptr; //NOLINT(readability-identifier-naming)

class EsModule;

class InternalBindingFunction {
public:
    InternalBindingFunction(v8::Local<v8::Context> context_) {
        const auto isolate  = context_->GetIsolate();
        const auto external = v8::External::New(isolate, this);
        const auto maybeFn  = v8::Function::New(
            context_,
            &Call,
            external,
            1);
        assert(!maybeFn.IsEmpty());
        v8::Local<v8::Function> fn;
        maybeFn.ToLocal(&fn);
        _fn.Reset(isolate, fn);
    }

    InternalBindingFunction(const InternalBindingFunction &) = delete;

    InternalBindingFunction(InternalBindingFunction &&) = delete;

    v8::Local<v8::Function> fn(v8::Isolate &isolate_) {
        return _fn.Get(&isolate_);
    }

    void add(const std::string &id_, v8::Local<v8::Value> value_, v8::Isolate *isolate_) {
        _bindings[id_].Reset(isolate_, value_);
    }

private:
    /// <summary>
    /// internalBinding(id: string): unknown
    /// </summary>
    /// <param name="args_"></param>
    static void Call(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        assert(args_.Data()->IsExternal());
        const auto external        = args_.Data().As<v8::External>();
        const auto internalBinding = reinterpret_cast<const InternalBindingFunction *>(external->Value());
        internalBinding->_call(args_);
    }

    v8::Persistent<v8::Function> _fn;

    std::map<std::string, v8::Persistent<v8::Value>> _bindings;

    void _call(const v8::FunctionCallbackInfo<v8::Value> &args_) const {
        const auto isolate = args_.GetIsolate();

        assert(args_.Length() == 1);

        assert(args_[0]->IsString());
        const auto id = v8::String::Utf8Value{isolate, args_[0].As<v8::String>()};

        const auto rBinding = _bindings.find(*id);
        if (rBinding == _bindings.end()) {
            std::ostringstream errMsg;
            errMsg << u8R"("Unknown internal binding: ")" << *id;
            isolate->ThrowException(v8::Exception::Error(v8::String::NewFromUtf8(isolate, errMsg.str().c_str()).ToLocalChecked()));
            args_.GetReturnValue().SetUndefined();
            return;
        }

        const auto &[_, binding] = *rBinding;
        args_.GetReturnValue().Set(binding.Get(isolate));
    }
};

class InternalBindingModule {
public:
    InternalBindingModule(
        v8::Local<v8::Context> context_, v8::Local<v8::Function> module_wrap_constructor_) : _internalBindings(context_) {
        auto isolate = context_->GetIsolate();

        const auto internalBindingExportName =
            v8::String::NewFromUtf8(isolate, exportNameInternalBinding).ToLocalChecked();
        std::vector<v8::Local<v8::Value>> exportNames{internalBindingExportName};

        const auto syntheticExecution =
            v8::Function::New(
                context_,
                [](const v8::FunctionCallbackInfo<v8::Value> &args_) {
                    assert(args_.Data()->IsExternal());
                    reinterpret_cast<InternalBindingModule *>(args_.Data().As<v8::External>()->Value())->_evaluate(args_);
                },
                v8::External::New(isolate, this))
                .ToLocalChecked();

        std::array<v8::Local<v8::Value>, 3> args{
            v8::String::NewFromUtf8(isolate, "internal_binding").ToLocalChecked(), // url
            v8::Array::New(isolate, exportNames.data(), exportNames.size()),       // exportNames
            syntheticExecution,                                                    // syntheticExecutionFunction
        };
        const auto moduleWrap = module_wrap_constructor_->CallAsConstructor(
                                                            context_,
                                                            args.size(),
                                                            args.data())
                                    .ToLocalChecked()
                                    .As<v8::Object>();
        this->_moduleWrap.Reset(isolate, moduleWrap);
    }

    InternalBindingModule(const InternalBindingModule &) = delete;

    InternalBindingModule(InternalBindingModule &&) = delete;

    v8::Local<v8::Object> module(v8::Isolate *isolate_) {
        return this->_moduleWrap.Get(isolate_);
    }

    InternalBindingFunction &bindings() {
        return _internalBindings;
    }

private:
    static constexpr auto exportNameInternalBinding = "internalBinding";

    v8::Persistent<v8::Object> _moduleWrap;

    InternalBindingFunction _internalBindings;

    void _evaluate(
        const v8::FunctionCallbackInfo<v8::Value> &args_) {
        auto       isolate                    = args_.GetIsolate();
        const auto context                    = isolate->GetCurrentContext();
        const auto moduleWrapJs               = args_.This();
        const auto setSyntheticModuleExportJs = moduleWrapJs->Get(
                                                                context,
                                                                v8::String::NewFromUtf8(isolate, u8R"(setSyntheticModuleExport)").ToLocalChecked())
                                                    .ToLocalChecked()
                                                    .As<v8::Function>();

        std::array<v8::Local<v8::Value>, 2> args{
            v8::String::NewFromUtf8(isolate, exportNameInternalBinding).ToLocalChecked(),
            _internalBindings.fn(*isolate)};
        setSyntheticModuleExportJs->CallAsFunction(
            context,
            moduleWrapJs,
            args.size(),
            args.data());
    }
};

enum ContextEmbedderIndex {
    environment = 32 + 10,
};

class EsEnvironment {
public:
    EsEnvironment(v8::Local<v8::Context> context_);

    EsEnvironment(const EsEnvironment &) = delete;

    EsEnvironment(EsEnvironment &&) = delete;

    v8::Local<v8::Context> context() {
        return _context.Get(_isolate);
    }

    v8::Local<v8::FunctionTemplate> createFunctionTemplate(
        v8::FunctionCallback     callback_,
        v8::Local<v8::Signature> signature_        = v8::Local<v8::Signature>(),
        v8::ConstructorBehavior  behavior_         = v8::ConstructorBehavior::kAllow,
        v8::SideEffectType       side_effect_type_ = v8::SideEffectType::kHasSideEffect) {
        v8::Local<v8::External> external = _external.Get(isolate());
        return v8::FunctionTemplate::New(
            _isolate,
            callback_,
            external,
            signature_,
            0,
            behavior_,
            side_effect_type_);
    }

    void SetMethod(v8::Local<v8::Object> that,
                   const char *          name,
                   v8::FunctionCallback  callback) {
        v8::Local<v8::Context>  context = isolate()->GetCurrentContext();
        v8::Local<v8::Function> function =
            createFunctionTemplate(callback, v8::Local<v8::Signature>(),
                                   // TODO(TimothyGu): Investigate if SetMethod is ever
                                   // used for constructors.
                                   v8::ConstructorBehavior::kAllow,
                                   v8::SideEffectType::kHasSideEffect)
                ->GetFunction(context)
                .ToLocalChecked();
        // kInternalized strings are created in the old space.
        const v8::NewStringType type = v8::NewStringType::kInternalized;
        v8::Local<v8::String>   name_string =
            v8::String::NewFromUtf8(isolate(), name, type).ToLocalChecked();
        that->Set(context, name_string, function).FromJust();
        function->SetName(name_string); // NODE_SET_METHOD() compatibility.
    }

    void SetProtoMethod(
        v8::Local<v8::FunctionTemplate> templ_,
        const char *                    name_,
        v8::FunctionCallback            callback_) {
        v8::Local<v8::Signature>        signature = v8::Signature::New(isolate(), templ_);
        v8::Local<v8::FunctionTemplate> t =
            createFunctionTemplate(callback_, signature, v8::ConstructorBehavior::kThrow,
                                   v8::SideEffectType::kHasSideEffect);
        // kInternalized strings are created in the old space.
        const v8::NewStringType type = v8::NewStringType::kInternalized;
        v8::Local<v8::String>   name_string =
            v8::String::NewFromUtf8(isolate(), name_, type).ToLocalChecked();
        templ_->PrototypeTemplate()->Set(name_string, t);
        t->SetClassName(name_string); // NODE_SET_PROTOTYPE_METHOD() compatibility.
    }

    void SetProtoMethodNoSideEffect(
        v8::Local<v8::FunctionTemplate> templ_,
        const char *                    name_,
        v8::FunctionCallback            callback_) {
        v8::Local<v8::Signature>        signature = v8::Signature::New(isolate(), templ_);
        v8::Local<v8::FunctionTemplate> t =
            createFunctionTemplate(callback_, signature, v8::ConstructorBehavior::kThrow,
                                   v8::SideEffectType::kHasNoSideEffect);
        // kInternalized strings are created in the old space.
        const v8::NewStringType type = v8::NewStringType::kInternalized;
        v8::Local<v8::String>   name_string =
            v8::String::NewFromUtf8(isolate(), name_, type).ToLocalChecked();
        templ_->PrototypeTemplate()->Set(name_string, t);
        t->SetClassName(name_string); // NODE_SET_PROTOTYPE_METHOD() compatibility.
    }

    void SetMethodNoSideEffect(v8::Local<v8::Object> templ_,
                               const char *          name_,
                               v8::FunctionCallback  callback) {
        v8::Local<v8::Context>  context = isolate()->GetCurrentContext();
        v8::Local<v8::Function> function =
            createFunctionTemplate(callback, v8::Local<v8::Signature>(),
                                   // TODO(TimothyGu): Investigate if SetMethod is ever
                                   // used for constructors.
                                   v8::ConstructorBehavior::kAllow,
                                   v8::SideEffectType::kHasNoSideEffect)
                ->GetFunction(context)
                .ToLocalChecked();
        // kInternalized strings are created in the old space.
        const v8::NewStringType type = v8::NewStringType::kInternalized;
        v8::Local<v8::String>   name_string =
            v8::String::NewFromUtf8(isolate(), name_, type).ToLocalChecked();
        templ_->Set(context, name_string, function).FromJust();
        function->SetName(name_string); // NODE_SET_METHOD() compatibility.
    }

    /// <summary>
    ///
    /// </summary>
    /// <param name="message_">UTF-8.</param>
    void throwError(const char *message_) {
        throwError(v8::Exception::Error, message_);
    }

    void throwError(
        v8::Local<v8::Value> (*constructor_)(v8::Local<v8::String>), const char *message_) {
        v8::HandleScope handleScope(isolate());
        (void)isolate()->ThrowException(constructor_(v8::String::NewFromUtf8(_isolate, message_).ToLocalChecked()));
    }

    void startup();

    ~EsEnvironment() {
        // TODO: SetAlignedPointerInEmbedderData to nullptr
    }

    v8::Isolate *isolate() {
        return _isolate;
    }

    /*void addCleanUpHook(std::function<void()> callback_) {
  _cleanupHooks.emplace(callback_);
}*/

    void registerModuleWrapper(v8::Local<v8::Module> module_, EsModule *wrapper_) {
        _hashToModuleMap.emplace(module_->GetIdentityHash(), wrapper_);
    }

    void unregisterModuleWrapper(v8::Local<v8::Module> module_, EsModule *wrapper_) {
        auto range = _hashToModuleMap.equal_range(module_->GetIdentityHash());
        for (auto i = range.first; i != range.second; ++i) {
            if (i->second == wrapper_) {
                _hashToModuleMap.erase(i);
                break;
            }
        }
    }

    EsModule *getModuleWrapper(v8::Local<v8::Module> module_);

    void runInInternal(const std::string &path_) {
        auto isolate = _isolate;

        auto               context = v8::Context::New(isolate);
        v8::Context::Scope contextScope{context};

        // Creates the global "exports" object.
        auto exports = v8::Object::New(isolate);
        context->Global()->Set(
            context,
            v8::String::NewFromUtf8(isolate, R"(exports)").ToLocalChecked(),
            exports);

        // Injects the "internalBindings" global function.
        context->Global()->Set(
            context,
            v8::String::NewFromUtf8(isolate, R"(internalBinding)").ToLocalChecked(),
            _internalBindingModule->bindings().fn(*isolate));

        const auto data               = v8::External::New(isolate, this);
        const auto loadInternalSource = v8::Function::New(context, _loadInternalSource, data).ToLocalChecked();
        context->Global()->Set(
            context,
            v8::String::NewFromUtf8(isolate, R"(loadInternalSource)").ToLocalChecked(),
            loadInternalSource);

        assert(_fileOperationDelegate.isValid());
        std::string sourceString = _fileOperationDelegate.onGetStringFromFile(path_);
        const auto  source       = v8::String::NewFromUtf8(isolate, sourceString.c_str()).ToLocalChecked();
        const auto  maybeScript  = v8::Script::Compile(
            context,
            source,
            nullptr);
        const auto       lineOffset         = v8::Integer::New(isolate, 0);
        const auto       columnOffset       = v8::Integer::New(isolate, 0);
        const auto       hostDefinedOptions = v8::PrimitiveArray::New(isolate, 0);
        v8::ScriptOrigin origin(
            v8::String::NewFromUtf8(isolate, path_.c_str()).ToLocalChecked(),
            lineOffset,
            columnOffset,
            v8::False(isolate),       // Is shared cross-origin
            v8::Local<v8::Integer>(), // Script id
            v8::Local<v8::Value>(),   // Source map url
            v8::False(isolate),       // Is opaque
            v8::False(isolate),       // Is Web assembly module
            v8::True(isolate),        // Is module,
            hostDefinedOptions);
        v8::Local<v8::Script> script;
        maybeScript.ToLocal(&script);
        script->Run(context);
    }

    v8::MaybeLocal<v8::Value> import(v8::Local<v8::String> specifier_, v8::MaybeLocal<v8::String> url_) {
        auto                 isolate           = _isolate;
        auto                 context           = _context.Get(isolate);
        const auto           loader            = _loader.Get(isolate);
        const auto           maybeImportMethod = loader->Get(context, v8::String::NewFromUtf8(isolate, "import").ToLocalChecked());
        v8::Local<v8::Value> importMethod;
        maybeImportMethod.ToLocal(&importMethod);
        std::array<v8::Local<v8::Value>, 2> args = {specifier_, url_.FromMaybe(v8::Undefined(isolate))};
        return importMethod.As<v8::Function>()->CallAsFunction(context, loader, args.size(), args.data());
    }

    static EsEnvironment *get(v8::Isolate *isolate_) {
        return get(isolate_->GetCurrentContext());
    }

    static EsEnvironment *get(v8::Local<v8::Context> context_) {
        return static_cast<EsEnvironment *>(
            context_->GetAlignedPointerFromEmbedderData(ContextEmbedderIndex::environment));
    }

    static EsEnvironment *get(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        assert(args_.Data()->IsExternal());
        return static_cast<EsEnvironment *>(args_.Data().As<v8::External>()->Value());
    }

    ScriptEngine::FileOperationDelegate _fileOperationDelegate;

private:
    v8::Isolate *                                 _isolate;
    v8::Persistent<v8::Context>                   _context;
    v8::Persistent<v8::External>                  _external;
    std::unordered_map<std::uint32_t, EsModule *> _hashToModuleMap;
    //std::unordered_set<std::function<void()>> _cleanupHooks; // TODO: clean
    v8::Persistent<v8::Function>           _promiseRejectCallback;
    std::unique_ptr<InternalBindingModule> _internalBindingModule;
    v8::Persistent<v8::FunctionTemplate>   _moduleWrapTempl;
    v8::Persistent<v8::Object>             _defaultLoader;
    v8::Persistent<v8::Object>             _loader;

    static void _onPromiseRejected(v8::PromiseRejectMessage message_) {
        auto promise = message_.GetPromise();
        auto isolate = promise->GetIsolate();
        auto event   = message_.GetEvent();

        auto env = EsEnvironment::get(isolate);
        if (env == nullptr) {
            return;
        }

        if (env->_promiseRejectCallback.IsEmpty()) {
            return;
        }

        v8::Local<v8::Value> value;

        switch (message_.GetEvent()) {
            case v8::PromiseRejectEvent::kPromiseRejectWithNoHandler:
                value = message_.GetValue();
                break;

            case v8::PromiseRejectEvent::kPromiseHandlerAddedAfterReject:
                break;

            case v8::PromiseRejectEvent::kPromiseRejectAfterResolved:
                break;

            case v8::PromiseRejectEvent::kPromiseResolveAfterResolved:
                break;

            default:
                assert(false || "Unreachable.");
                break;
        }

        if (value.IsEmpty()) {
            value = v8::Undefined(isolate);
        }

        std::array<v8::Local<v8::Value>, 3> args = {
            promise,
            v8::Number::New(isolate, event),
            value};
        env->_promiseRejectCallback.Get(isolate)->Call(
            env->context(), v8::Undefined(isolate), args.size(), args.data());
    }

    static void _handleException(EsEnvironment *env_, v8::Local<v8::Value> exception_) {
        const auto context = env_->context();
        const auto isolate = env_->isolate();
        /*auto global = context->Global();
    auto console = global->Get(v8::String::NewFromUtf8(isolate, "console")).As<v8::Object>();
    auto error = console->Get(v8::String::NewFromUtf8(isolate, "error")).As<v8::Function>();
    v8::Local<v8::Value> ret;
    error->Call(context, console, 1, &exception_);*/
        if (exception_->IsObject()) {
            const auto maybeMessage = exception_.As<v8::Object>()->Get(context, v8::String::NewFromUtf8(isolate, "message").ToLocalChecked());
            if (!maybeMessage.IsEmpty()) {
                v8::Local<v8::Value> message;
                maybeMessage.ToLocal(&message);
                if (message->IsString()) {
                    const auto            messageText = message.As<v8::String>();
                    v8::String::Utf8Value messageTextU8(isolate, messageText);
                    std::string           messageTextNative(*messageTextU8, messageTextU8.length());
                    SE_LOGE("%s", messageTextNative.c_str());
                }
            }
        }
    }

    static void SetPromiseRejectCallback(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        auto env = EsEnvironment::get(args_);

        assert(args_[0]->IsFunction());
        env->_promiseRejectCallback.Reset(env->_isolate, args_[0].As<v8::Function>());
    }

    static void _loadInternalSource(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        auto isolate = args_.GetIsolate();
        auto env     = EsEnvironment::get(args_);

        assert(args_.Length() == 1);
        assert(args_[0]->IsString());

        v8::String::Utf8Value sourceId{isolate, args_[0]};
        std::string           path   = "libs/" + std::string{*sourceId};
        const auto            source = env->_fileOperationDelegate.onGetStringFromFile(path);
        args_.GetReturnValue().Set(v8::String::NewFromUtf8(isolate, source.data()).ToLocalChecked());
    }

    std::string _loadDefaultLoaderScriptSource(const std::string &path_) {
        assert(_fileOperationDelegate.isValid());

        std::string scriptBuffer = _fileOperationDelegate.onGetStringFromFile(path_);

        return scriptBuffer;
    }

    void _setLoader(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        assert(args_.Length() == 1);
        assert(args_[0]->IsObject());
        auto loader = v8::Local<v8::Object>::Cast(args_[0]);
        _loader.Reset(_isolate, loader);
    }
};

class EsObject {
public:
    EsObject(EsEnvironment *       env_,
             v8::Local<v8::Object> object_) : _env(env_), _handle(env_->isolate(), object_) {
        assert(!object_.IsEmpty());
        assert(object_->InternalFieldCount() > 0);
        object_->SetAlignedPointerInInternalField(0, static_cast<void *>(this));
    }

    ~EsObject() {
        if (_handle.IsEmpty()) {
            return;
        }

        v8::HandleScope handleScope(_env->isolate());
        object()->SetAlignedPointerInInternalField(0, nullptr);
    }

    EsEnvironment *env() {
        return _env;
    }

    v8::Local<v8::Object> object() {
        return _handle.Get(_env->isolate());
    }

    static EsObject *unwrap(v8::Local<v8::Object> object_) {
        assert(object_->InternalFieldCount() > 0);
        return static_cast<EsObject *>(object_->GetAlignedPointerFromInternalField(0));
    }

    template <typename Derived>
    static Derived *unwrap(v8::Local<v8::Object> object_) {
        return static_cast<Derived *>(unwrap(object_));
    }

private:
    EsEnvironment *            _env;
    v8::Persistent<v8::Object> _handle;
};

class EsModule : public EsObject {
public:
    /// <summary>
    ///
    /// </summary>
    /// <param name="env_"></param>
    /// <param name="object_">The Module `this` object.</param>
    /// <param name="module_">The v8 module object.</param>
    /// <param name="url_">The url of the module.</param>
    EsModule(EsEnvironment *        env_,
             v8::Local<v8::Object>  object_,
             v8::Local<v8::Module>  module_,
             v8::Local<v8::String>  url_,
             v8::Local<v8::Context> context_) : EsObject(env_, object_),
                                                _url(env_->isolate(), url_),
                                                _module(env_->isolate(), module_),
                                                _context(env_->isolate(), context_) {
        env_->registerModuleWrapper(module_, this);
    }

    ~EsModule() {
        v8::HandleScope scope(env()->isolate());
        auto            module = _module.Get(env()->isolate());
        env()->unregisterModuleWrapper(module, this);
    }

    v8::Local<v8::Module> module(v8::Isolate *isolate_) {
        return this->_module.Get(isolate_);
    }

    v8::Local<v8::String> url() {
        return _url.Get(env()->isolate());
    }

    static v8::Local<v8::FunctionTemplate> createFunctionTemplate(EsEnvironment &env_) {
        const auto isoate = env_.isolate();
        auto       templ  = env_.createFunctionTemplate(EsModule::New);
        templ->SetClassName(v8::String::NewFromUtf8(isoate, "EsModule").ToLocalChecked());
        templ->InstanceTemplate()->SetInternalFieldCount(InternalFieldSlots::count);
        env_.SetProtoMethod(templ, "link", EsModule::Link);
        env_.SetProtoMethod(templ, "instantiate", EsModule::Instantiate);
        env_.SetProtoMethod(templ, "evaluate", EsModule::Evaluate);
        env_.SetProtoMethod(templ, "setSyntheticModuleExport", EsModule::SetSyntheticModuleExport);
        env_.SetProtoMethod(templ, "namespace", EsModule::Namespace);
        return templ;
    }

private:
    enum InternalFieldSlots {
        syntheticEvaluationSteps = 1,
        count,
    };

    /// <summary>
    /// new ModuleWrap(url: string, source: string, lineOffset: number, columnOffset: number)
    /// new ModuleWrap(url: string, exportNames: string[], syntheticExecutionFunction: Function)
    /// </summary>
    /// <param name="args_"></param>
    static void New(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        assert(args_.IsConstructCall());
        const auto env     = EsEnvironment::get(args_);
        const auto isolate = env->isolate();

        const auto jsThis = args_.This();

        const auto argc = args_.Length();
        assert(argc >= 2);

        assert(args_[0]->IsString());
        const auto url = args_[0].As<v8::String>();

        v8::Local<v8::Context> context = jsThis->CreationContext();

        v8::Local<v8::Module> module;
        const auto &          arg1 = args_[1];
        if (!arg1->IsString()) {
            assert(arg1->IsArray());
            assert(args_[2]->IsFunction());
            const auto                         exportNamesJs = arg1.As<v8::Array>();
            const auto                         nExportNames  = exportNamesJs->Length();
            std::vector<v8::Local<v8::String>> exportNames(nExportNames);
            for (std::remove_const_t<decltype(nExportNames)> iExportName = 0;
                 iExportName < nExportNames; ++iExportName) {
                const auto exportNameJs = exportNamesJs->Get(context, iExportName).ToLocalChecked();
                assert(exportNameJs->IsString());
                exportNames[iExportName] = exportNameJs.As<v8::String>();
            }
            module = v8::Module::CreateSyntheticModule(
                isolate,
                url,
                exportNames,
                _evaluationSteps);
            jsThis->SetInternalField(InternalFieldSlots::syntheticEvaluationSteps, args_[2]);
        } else {
            const auto sourceText = arg1.As<v8::String>();

            v8::TryCatch tryCatch(isolate);

            const auto lineOffset         = v8::Integer::New(isolate, 0);
            const auto columnOffset       = v8::Integer::New(isolate, 0);
            const auto hostDefinedOptions = v8::PrimitiveArray::New(isolate, 0);

            v8::ScriptOrigin origin(
                url,
                lineOffset,
                columnOffset,
                v8::False(isolate),       // Is shared cross-origin
                v8::Local<v8::Integer>(), // Script id
                v8::Local<v8::Value>(),   // Source map url
                v8::False(isolate),       // Is opaque
                v8::False(isolate),       // Is Web assembly module
                v8::True(isolate),        // Is module,
                hostDefinedOptions);
            v8::Context::Scope         contextScope(context);
            v8::ScriptCompiler::Source source(sourceText, origin);
            if (!v8::ScriptCompiler::CompileModule(isolate, &source).ToLocal(&module)) {
                assert(tryCatch.HasCaught());
                assert(!tryCatch.Message().IsEmpty());
                assert(!tryCatch.Exception().IsEmpty());
                tryCatch.ReThrow();
                return;
            }
        }

        const auto esModule = new EsModule(env, jsThis, module, url, context);
        args_.GetReturnValue().Set(jsThis);
    }

    static void Link(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        const auto env     = EsEnvironment::get(args_);
        const auto isolate = env->isolate();

        assert(args_.Length() == 1);
        assert(args_[0]->IsFunction());

        const auto jsThis        = args_.This();
        const auto moduleWrapped = EsModule::unwrap<EsModule>(jsThis);

        if (moduleWrapped->_linked) {
            return;
        }
        moduleWrapped->_linked = true;

        const auto resolver = args_[0].As<v8::Function>();

        const auto context = moduleWrapped->_context.Get(isolate);
        const auto module  = moduleWrapped->_module.Get(isolate);

        const auto nModuleRequests = module->GetModuleRequestsLength();
        auto       resolvePromises = v8::Array::New(isolate, nModuleRequests);
        for (std::remove_const_t<decltype(nModuleRequests)> iModuleRequest = 0;
             iModuleRequest < nModuleRequests; ++iModuleRequest) {
            auto                                moduleRequest   = module->GetModuleRequest(iModuleRequest);
            std::array<v8::Local<v8::Value>, 1> resolveArgs     = {moduleRequest};
            auto                                resolverRetvalX = resolver->Call(context, jsThis, resolveArgs.size(), resolveArgs.data());
            if (resolverRetvalX.IsEmpty()) {
                // TODO?
                return;
            }
            auto resolvePromiseX = resolverRetvalX.ToLocalChecked();
            if (!resolvePromiseX->IsPromise()) {
                // TODO: throw
            }
            auto resolvePromise = resolvePromiseX.As<v8::Promise>();

            v8::String::Utf8Value moduleRequestU8(env->isolate(), moduleRequest);
            std::string           moduleRequestNative(*moduleRequestU8, moduleRequestU8.length());
            moduleWrapped->_resolvePromises[moduleRequestNative].Reset(env->isolate(), resolvePromise);

            resolvePromises->Set(context, iModuleRequest, resolvePromise).FromJust();
        }

        args_.GetReturnValue().Set(resolvePromises);
    }

    static void Instantiate(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        auto env     = EsEnvironment::get(args_);
        auto isolate = env->isolate();

        auto jsThis        = args_.This();
        auto moduleWrapped = EsModule::unwrap<EsModule>(jsThis);

        auto context = moduleWrapped->_context.Get(isolate);
        auto module  = moduleWrapped->_module.Get(isolate);

        v8::TryCatch tryCatch(env->isolate());
        auto         result = module->InstantiateModule(context, ResolveCallback);

        if (!result.FromMaybe(false)) {
            assert(tryCatch.HasCaught());
            assert(!tryCatch.Message().IsEmpty());
            assert(!tryCatch.Exception().IsEmpty());
            tryCatch.ReThrow();
            return;
        }
    }

    static void Evaluate(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        auto env     = EsEnvironment::get(args_);
        auto isolate = env->isolate();

        auto jsThis        = args_.This();
        auto moduleWrapped = EsModule::unwrap<EsModule>(jsThis);

        seLogE("Evaluating %s\n", *v8::String::Utf8Value(isolate, moduleWrapped->url()));

        auto context = moduleWrapped->_context.Get(isolate);
        auto module  = moduleWrapped->_module.Get(isolate);

        v8::TryCatch tryCatch(env->isolate());

        auto result = module->Evaluate(context);

        if (tryCatch.HasCaught()) {
            tryCatch.ReThrow();
            return;
        }

        args_.GetReturnValue().Set(result.ToLocalChecked());
    }

    static void SetSyntheticModuleExport(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        auto env        = EsEnvironment::get(args_);
        auto isolate    = env->isolate();
        auto jsThis     = args_.This();
        auto moduleWrap = EsModule::unwrap<EsModule>(jsThis);

        assert(args_.Length() == 2);
        assert(args_[0]->IsString());

        const auto exportName  = args_[0].As<v8::String>();
        const auto exportValue = args_[1];
        const auto v8Module    = moduleWrap->_module.Get(isolate);
        v8Module->SetSyntheticModuleExport(exportName, exportValue);
    }

    static void Namespace(const v8::FunctionCallbackInfo<v8::Value> &args_) {
        auto env           = EsEnvironment::get(args_);
        auto isolate       = env->isolate();
        auto jsThis        = args_.This();
        auto moduleWrapped = EsModule::unwrap<EsModule>(jsThis);
        auto module        = moduleWrapped->_module.Get(isolate);
        args_.GetReturnValue().Set(module->GetModuleNamespace());
    }

    static v8::MaybeLocal<v8::Module> ResolveCallback(
        v8::Local<v8::Context> context_,
        v8::Local<v8::String>  module_request_,
        v8::Local<v8::Module>  importer_) {
        auto env     = EsEnvironment::get(context_);
        auto isolate = env->isolate();

        auto importerWrapper = env->getModuleWrapper(importer_);
        assert(importerWrapper);

        v8::String::Utf8Value moduleRequestU8(isolate, module_request_);
        std::string           moduleRequestNative(*moduleRequestU8, moduleRequestU8.length());
        // TODO: what if same specifier occurs multi times
        if (importerWrapper->_resolvePromises.count(moduleRequestNative) != 1) {
            env->throwError(u8"Linking was not performend prior to instantiate.");
            return {};
        }

        const auto resolvePromise = importerWrapper->_resolvePromises[moduleRequestNative].Get(isolate);
        if (resolvePromise->State() != v8::Promise::kFulfilled) {
            env->throwError(u8"Linking promise was not fulfilled at the moment the module instantiates.");
            return {};
        }

        auto resolvedModuleX = resolvePromise->Result();
        if (resolvedModuleX.IsEmpty() ||
            !resolvedModuleX->IsObject()) {
            env->throwError(u8"Linking promise did not result a module object.");
            return {};
        }

        auto resolvedModuleWrapped = EsModule::unwrap<EsModule>(resolvedModuleX.As<v8::Object>());
        if (!resolvedModuleWrapped) {
            // TODO THROW: not wrapped
            return {};
        }

        return resolvedModuleWrapped->_module.Get(isolate);
    }

    static v8::MaybeLocal<v8::Value> _evaluationSteps(
        v8::Local<v8::Context> context_, v8::Local<v8::Module> module_) {
        auto env     = EsEnvironment::get(context_);
        auto isolate = env->isolate();

        auto importerWrapper = env->getModuleWrapper(module_);
        assert(importerWrapper);

        v8::TryCatch tryCatch{isolate};

        const auto syntheticEvaluationSteps =
            importerWrapper->object()->GetInternalField(InternalFieldSlots::syntheticEvaluationSteps).As<v8::Function>();
        const auto evaluationResult = syntheticEvaluationSteps->Call(context_, importerWrapper->object(), 0, nullptr);
        if (evaluationResult.IsEmpty()) {
            assert(tryCatch.HasCaught());
        }

        if (tryCatch.HasCaught() && !tryCatch.HasTerminated()) {
            assert(!tryCatch.Message().IsEmpty());
            assert(!tryCatch.Exception().IsEmpty());
            tryCatch.ReThrow();
            return v8::MaybeLocal<v8::Value>();
        }

        return v8::Undefined(isolate);
    }

    v8::Persistent<v8::Module>                                   _module;
    v8::Persistent<v8::String>                                   _url;
    v8::Persistent<v8::Context>                                  _context;
    std::unordered_map<std::string, v8::Persistent<v8::Promise>> _resolvePromises;
    bool                                                         _linked = false;
};

EsEnvironment::EsEnvironment(v8::Local<v8::Context> context_) : _isolate(context_->GetIsolate()), _context(context_->GetIsolate(), context_) {
    context_->SetAlignedPointerInEmbedderData(ContextEmbedderIndex::environment, this);
    _external.Reset(_isolate, v8::External::New(_isolate, this));
    // _isolate->SetPromiseRejectCallback(_onPromiseRejected);

    const auto context = _context.Get(_isolate);

    _moduleWrapTempl.Reset(_isolate, EsModule::createFunctionTemplate(*this));

    const auto moduleWrapConstructor = _moduleWrapTempl.Get(_isolate)->GetFunction(context).ToLocalChecked();
    _internalBindingModule           = std::make_unique<InternalBindingModule>(context, moduleWrapConstructor);
}

EsModule *EsEnvironment::getModuleWrapper(v8::Local<v8::Module> module_) {
    auto range = _hashToModuleMap.equal_range(module_->GetIdentityHash());
    for (auto i = range.first; i != range.second; ++i) {
        if (i->second->module(this->_isolate) == module_) {
            return i->second;
        }
    }
    return nullptr;
}

template <typename T>
T &get_native(const v8::FunctionCallbackInfo<v8::Value> &args_) {
    assert(args_.Data()->IsExternal());
    return *static_cast<T *>(args_.Data().As<v8::External>()->Value());
}

void EsEnvironment::startup() {
    auto isolate = _isolate;
    auto context = _context.Get(_isolate);
    auto global  = context->Global();

    auto target = global;

    auto       moduleWrapTempl = _moduleWrapTempl.Get(isolate);
    const auto moduleWrap      = moduleWrapTempl->GetFunction(context).ToLocalChecked();
    target->Set(
              this->context(),
              v8::String::NewFromUtf8(_isolate, "EsModule").ToLocalChecked(),
              moduleWrap)
        .FromJust();

    this->SetMethod(
        target,
        "setPromiseRejectCallback",
        SetPromiseRejectCallback);

    _internalBindingModule->bindings().add(u8R"(ModuleWrap)", moduleWrap, context->GetIsolate());

    _internalBindingModule->bindings().add(u8R"(internalBindingModuleWrap)", _internalBindingModule->module(isolate), isolate);

    _internalBindingModule->bindings().add(
        u8R"(log)",
        v8::Function::New(
            context,
            [](const v8::FunctionCallbackInfo<v8::Value> &args_) {
                assert(args_.Length() == 1);
                assert(args_[0]->IsString());
                const auto message = v8::String::Utf8Value(args_.GetIsolate(), args_[0]);
                seLogE("[internalBinding.log] %s\n", *message);
            },
            v8::External::New(isolate, this))
            .ToLocalChecked(),
        isolate);

    _internalBindingModule->bindings().add(
        u8R"(getStringFromFile)",
        v8::Function::New(
            context,
            [](const v8::FunctionCallbackInfo<v8::Value> &args_) {
                assert(args_.Length() == 1);
                assert(args_[0]->IsString());
                const auto path   = v8::String::Utf8Value(args_.GetIsolate(), args_[0]);
                const auto env    = static_cast<EsEnvironment *>(args_.Data().As<v8::External>()->Value());
                const auto source = env->_fileOperationDelegate.onGetStringFromFile(*path);
                args_.GetReturnValue().Set(v8::String::NewFromUtf8(args_.GetIsolate(), source.data()).ToLocalChecked());
            },
            v8::External::New(isolate, this))
            .ToLocalChecked(),
        isolate);

    _internalBindingModule->bindings().add(
        u8R"(setLoader)",
        v8::Function::New(
            context,
            [](const v8::FunctionCallbackInfo<v8::Value> &args_) {
                EsEnvironment::get(args_)->_setLoader(args_);
            },
            v8::External::New(isolate, this))
            .ToLocalChecked(),
        isolate);
}

namespace {
ScriptEngine *gSriptEngineInstance = nullptr;

void seLogCallback(const v8::FunctionCallbackInfo<v8::Value> &info) {
    if (info[0]->IsString()) {
        v8::String::Utf8Value utf8(v8::Isolate::GetCurrent(), info[0]);
        SE_LOGD("JS: %s\n", *utf8);
    }
}

void seForceGC(const v8::FunctionCallbackInfo<v8::Value> & /*info*/) {
    ScriptEngine::getInstance()->garbageCollect();
}

std::string stackTraceToString(v8::Local<v8::StackTrace> stack) {
    std::string stackStr;
    if (stack.IsEmpty()) {
        return stackStr;
    }

    char tmp[100] = {0};
    for (int i = 0, e = stack->GetFrameCount(); i < e; ++i) {
        v8::Local<v8::StackFrame> frame  = stack->GetFrame(v8::Isolate::GetCurrent(), i);
        v8::Local<v8::String>     script = frame->GetScriptName();
        std::string               scriptName;
        if (!script.IsEmpty()) {
            scriptName = *v8::String::Utf8Value(v8::Isolate::GetCurrent(), script);
        }

        v8::Local<v8::String> func = frame->GetFunctionName();
        std::string           funcName;
        if (!func.IsEmpty()) {
            funcName = *v8::String::Utf8Value(v8::Isolate::GetCurrent(), func);
        }

        stackStr += " - [";
        snprintf(tmp, sizeof(tmp), "%d", i);
        stackStr += tmp;
        stackStr += "]";
        stackStr += (funcName.empty() ? "anonymous" : funcName.c_str());
        stackStr += "@";
        stackStr += (scriptName.empty() ? "(no filename)" : scriptName.c_str());
        stackStr += ":";
        snprintf(tmp, sizeof(tmp), "%d", frame->GetLineNumber());
        stackStr += tmp;

        if (i < (e - 1)) {
            stackStr += "\n";
        }
    }

    return stackStr;
}

se::Value oldConsoleLog;
se::Value oldConsoleDebug;
se::Value oldConsoleInfo;
se::Value oldConsoleWarn;
se::Value oldConsoleError;
se::Value oldConsoleAssert;

bool jsbConsoleFormatLog(State &state, const char *prefix, int msgIndex = 0) {
    if (msgIndex < 0) {
        return false;
    }

    const auto &args = state.args();
    int         argc = static_cast<int>(args.size());
    if ((argc - msgIndex) == 1) {
        std::string msg = args[msgIndex].toStringForce();
        SE_LOGD("JS: %s%s\n", prefix, msg.c_str());
    } else if (argc > 1) {
        std::string msg = args[msgIndex].toStringForce();
        size_t      pos;
        for (int i = (msgIndex + 1); i < argc; ++i) {
            pos = msg.find('%');
            if (pos != std::string::npos && pos != (msg.length() - 1) && (msg[pos + 1] == 'd' || msg[pos + 1] == 's' || msg[pos + 1] == 'f')) {
                msg.replace(pos, 2, args[i].toStringForce());
            } else {
                msg += " " + args[i].toStringForce();
            }
        }

        SE_LOGD("JS: %s%s\n", prefix, msg.c_str());
    }

    return true;
}

bool jsbConsoleLog(State &s) {
    jsbConsoleFormatLog(s, "");
    oldConsoleLog.toObject()->call(s.args(), s.thisObject());
    return true;
}
SE_BIND_FUNC(jsbConsoleLog)

bool jsbConsoleDebug(State &s) {
    jsbConsoleFormatLog(s, "[DEBUG]: ");
    oldConsoleDebug.toObject()->call(s.args(), s.thisObject());
    return true;
}
SE_BIND_FUNC(jsbConsoleDebug)

bool jsbConsoleInfo(State &s) {
    jsbConsoleFormatLog(s, "[INFO]: ");
    oldConsoleInfo.toObject()->call(s.args(), s.thisObject());
    return true;
}
SE_BIND_FUNC(jsbConsoleInfo)

bool jsbConsoleWarn(State &s) {
    jsbConsoleFormatLog(s, "[WARN]: ");
    oldConsoleWarn.toObject()->call(s.args(), s.thisObject());
    return true;
}
SE_BIND_FUNC(jsbConsoleWarn)

bool jsbConsoleError(State &s) {
    jsbConsoleFormatLog(s, "[ERROR]: ");
    oldConsoleError.toObject()->call(s.args(), s.thisObject());
    return true;
}
SE_BIND_FUNC(jsbConsoleError)

bool jsbConsoleAssert(State &s) {
    const auto &args = s.args();
    if (!args.empty()) {
        if (args[0].isBoolean() && !args[0].toBoolean()) {
            jsbConsoleFormatLog(s, "[ASSERT]: ", 1);
            oldConsoleAssert.toObject()->call(s.args(), s.thisObject());
        }
    }
    return true;
}
SE_BIND_FUNC(jsbConsoleAssert)

/*
        * The unique V8 platform instance
        */
class ScriptEngineV8Context {
public:
    ScriptEngineV8Context() {
        platform = v8::platform::NewDefaultPlatform().release();
        v8::V8::InitializePlatform(platform);

        std::string flags;
        //NOTICE: spaces are required between flags
        flags.append(" --expose-gc-as=" EXPOSE_GC);
        flags.append(" --no-flush-bytecode --no-lazy"); // for bytecode support
                                                        // flags.append(" --trace-gc"); // v8 trace gc
    #if (CC_PLATFORM == CC_PLATFORM_MAC_IOS)
        flags.append(" --jitless");
    #endif
        if (!flags.empty()) {
            v8::V8::SetFlagsFromString(flags.c_str(), static_cast<int>(flags.length()));
        }

        bool ok = v8::V8::Initialize();
        assert(ok);
    }

    ~ScriptEngineV8Context() {
        v8::V8::Dispose();
        v8::V8::ShutdownPlatform();
        delete platform;
    }
    v8::Platform *platform = nullptr;
};

ScriptEngineV8Context *gSharedV8 = nullptr;

} // namespace

void ScriptEngine::callExceptionCallback(const char *location, const char *message, const char *stack) {
    if (_nativeExceptionCallback) {
        _nativeExceptionCallback(location, message, stack);
    }
    if (_jsExceptionCallback) {
        _jsExceptionCallback(location, message, stack);
    }
}

void ScriptEngine::onFatalErrorCallback(const char *location, const char *message) {
    std::string errorStr = "[FATAL ERROR] location: ";
    errorStr += location;
    errorStr += ", message: ";
    errorStr += message;

    SE_LOGE("%s\n", errorStr.c_str());

    getInstance()->callExceptionCallback(location, message, "(no stack information)");
}

void ScriptEngine::onOOMErrorCallback(const char *location, bool isHeapOom) {
    std::string errorStr = "[OOM ERROR] location: ";
    errorStr += location;
    std::string message;
    message = "is heap out of memory: ";
    if (isHeapOom) {
        message += "true";
    } else {
        message += "false";
    }

    errorStr += ", " + message;
    SE_LOGE("%s\n", errorStr.c_str());
    getInstance()->callExceptionCallback(location, message.c_str(), "(no stack information)");
}

void ScriptEngine::onMessageCallback(v8::Local<v8::Message> message, v8::Local<v8::Value> /*data*/) {
    ScriptEngine *        thiz = getInstance();
    v8::Local<v8::String> msg  = message->Get();
    Value                 msgVal;
    internal::jsToSeValue(v8::Isolate::GetCurrent(), msg, &msgVal);
    assert(msgVal.isString());
    v8::ScriptOrigin origin = message->GetScriptOrigin();
    Value            resouceNameVal;
    internal::jsToSeValue(v8::Isolate::GetCurrent(), origin.ResourceName(), &resouceNameVal);
    Value line;
    internal::jsToSeValue(v8::Isolate::GetCurrent(), origin.ResourceLineOffset(), &line);
    Value column;
    internal::jsToSeValue(v8::Isolate::GetCurrent(), origin.ResourceColumnOffset(), &column);

    std::string location = resouceNameVal.toStringForce() + ":" + line.toStringForce() + ":" + column.toStringForce();

    std::string errorStr = msgVal.toString() + ", location: " + location;
    std::string stackStr = stackTraceToString(message->GetStackTrace());
    if (!stackStr.empty()) {
        if (line.toInt32() == 0) {
            location = "(see stack)";
        }
        errorStr += "\nSTACK:\n" + stackStr;
    }
    SE_LOGE("ERROR: %s\n", errorStr.c_str());

    thiz->callExceptionCallback(location.c_str(), msgVal.toString().c_str(), stackStr.c_str());

    if (!thiz->_isErrorHandleWorking) {
        thiz->_isErrorHandleWorking = true;

        Value errorHandler;
        if (thiz->_globalObj && thiz->_globalObj->getProperty("__errorHandler", &errorHandler) && errorHandler.isObject() && errorHandler.toObject()->isFunction()) {
            ValueArray args;
            args.push_back(resouceNameVal);
            args.push_back(line);
            args.push_back(msgVal);
            args.push_back(Value(stackStr));
            errorHandler.toObject()->call(args, thiz->_globalObj);
        }

        thiz->_isErrorHandleWorking = false;
    } else {
        SE_LOGE("ERROR: __errorHandler has exception\n");
    }
}

void ScriptEngine::onPromiseRejectCallback(v8::PromiseRejectMessage msg) {
    v8::Isolate *     isolate = getInstance()->_isolate;
    v8::HandleScope   scope(isolate);
    std::stringstream ss;
    auto              event       = msg.GetEvent();
    auto              value       = msg.GetValue();
    auto              promiseName = msg.GetPromise()->GetConstructorName();
    const char *      eventName   = "[invalidatePromiseEvent]";

    if (event == v8::kPromiseRejectWithNoHandler) {
        eventName = "unhandledRejectedPromise";
    } else if (event == v8::kPromiseHandlerAddedAfterReject) {
        eventName = "handlerAddedAfterPromiseRejected";
    } else if (event == v8::kPromiseRejectAfterResolved) {
        eventName = "rejectAfterPromiseResolved";
    } else if (event == v8::kPromiseResolveAfterResolved) {
        eventName = "resolveAfterPromiseResolved";
    }

    if (!value.IsEmpty()) {
        // prepend error object to stack message
        v8::Local<v8::String> str = value->ToString(isolate->GetCurrentContext()).ToLocalChecked();
        v8::String::Utf8Value valueUtf8(isolate, str);
        auto *                strp = *valueUtf8;
        if (strp == nullptr) {
            ss << "value: null" << std::endl;
            auto                  tn = value->TypeOf(isolate);
            v8::String::Utf8Value tnUtf8(isolate, tn);
            strp = *tnUtf8;
            if (strp) {
                ss << " type: " << strp << std::endl;
            }
            if (value->IsObject()) {
                v8::MaybeLocal<v8::String> json = v8::JSON::Stringify(isolate->GetCurrentContext(), value);
                if (!json.IsEmpty()) {
                    v8::String::Utf8Value jsonStr(isolate, json.ToLocalChecked());
                    strp = *jsonStr;
                    if (strp) {
                        ss << " obj: " << strp << std::endl;
                    } else {
                        ss << " obj: null" << std::endl;
                    }
                } else {
                    v8::Local<v8::Object> obj       = value->ToObject(isolate->GetCurrentContext()).ToLocalChecked();
                    v8::Local<v8::Array>  attrNames = obj->GetOwnPropertyNames(isolate->GetCurrentContext()).ToLocalChecked();

                    if (!attrNames.IsEmpty()) {
                        uint32_t size = attrNames->Length();

                        for (uint32_t i = 0; i < size; i++) {
                            se::Value             e;
                            v8::Local<v8::String> attrName = attrNames->Get(isolate->GetCurrentContext(), i)
                                                                 .ToLocalChecked()
                                                                 ->ToString(isolate->GetCurrentContext())
                                                                 .ToLocalChecked();
                            v8::String::Utf8Value attrUtf8(isolate, attrName);
                            strp = *attrUtf8;
                            ss << " obj.property " << strp << std::endl;
                        }
                        ss << " obj: JSON.parse failed!" << std::endl;
                    }
                }
            }

        } else {
            ss << *valueUtf8 << std::endl;
        }

        v8::String::Utf8Value valuePromiseConstructor(isolate, promiseName);
        strp = *valuePromiseConstructor;
        if (strp) {
            ss << "PromiseConstructor " << strp;
        }
    }

    auto stackStr = getInstance()->getCurrentStackTrace();
    ss << "stacktrace: " << std::endl;
    ss << stackStr << std::endl;
    getInstance()->callExceptionCallback("", eventName, ss.str().c_str());
}

void ScriptEngine::privateDataFinalize(void *nativeObj) {
    auto *p = static_cast<internal::PrivateData *>(nativeObj);

    Object::nativeObjectFinalizeHook(p->data);

    assert(p->seObj->getRefCount() == 1);

    p->seObj->decRef();

    free(p);
}

ScriptEngine *ScriptEngine::getInstance() {
    if (gSriptEngineInstance == nullptr) {
        gSriptEngineInstance = new ScriptEngine();
    }

    return gSriptEngineInstance;
}

void ScriptEngine::destroyInstance() {
    if (gSriptEngineInstance) {
        gSriptEngineInstance->cleanup();
        delete gSriptEngineInstance;
        gSriptEngineInstance = nullptr;
    }
}

ScriptEngine::ScriptEngine()
: _isolate(nullptr),
  _handleScope(nullptr),
  _globalObj(nullptr)
    #if SE_ENABLE_INSPECTOR
  ,
  _env(nullptr),
  _isolateData(nullptr)
    #endif
  ,
  _debuggerServerPort(0),
  _vmId(0),
  _isValid(false),
  _isGarbageCollecting(false),
  _isInCleanup(false),
  _isErrorHandleWorking(false) {

    if (!gSharedV8) {
        gSharedV8 = new ScriptEngineV8Context();
    }
}

ScriptEngine::~ScriptEngine() = default;

bool ScriptEngine::init() {
    cleanup();
    SE_LOGD("Initializing V8, version: %s\n", v8::V8::GetVersion());
    ++_vmId;

    _engineThreadId = std::this_thread::get_id();

    for (const auto &hook : _beforeInitHookArray) {
        hook();
    }
    _beforeInitHookArray.clear();
    v8::Isolate::CreateParams createParams;
    createParams.array_buffer_allocator = v8::ArrayBuffer::Allocator::NewDefaultAllocator();
    _isolate                            = v8::Isolate::New(createParams);
    v8::HandleScope hs(_isolate);
    _isolate->Enter();

    _isolate->SetCaptureStackTraceForUncaughtExceptions(true, JSB_STACK_FRAME_LIMIT, v8::StackTrace::kOverview);

    _isolate->SetFatalErrorHandler(onFatalErrorCallback);
    _isolate->SetOOMErrorHandler(onOOMErrorCallback);
    _isolate->AddMessageListener(onMessageCallback);
    _isolate->SetPromiseRejectCallback(onPromiseRejectCallback);

    _context.Reset(_isolate, v8::Context::New(_isolate));
    _context.Get(_isolate)->Enter();

    _environment                         = std::make_unique<EsEnvironment>(_context.Get(_isolate));
    _environment->_fileOperationDelegate = _fileOperationDelegate;
    _environment->startup();

    NativePtrToObjectMap::init();
    NonRefNativePtrCreatedByCtorMap::init();

    Object::setup();
    Class::setIsolate(_isolate);
    Object::setIsolate(_isolate);

    _globalObj = Object::_createJSObject(nullptr, _context.Get(_isolate)->Global());
    _globalObj->root();
    _globalObj->setProperty("window", Value(_globalObj));

    se::Value consoleVal;
    if (_globalObj->getProperty("console", &consoleVal) && consoleVal.isObject()) {
        consoleVal.toObject()->getProperty("log", &oldConsoleLog);
        consoleVal.toObject()->defineFunction("log", _SE(jsbConsoleLog));

        consoleVal.toObject()->getProperty("debug", &oldConsoleDebug);
        consoleVal.toObject()->defineFunction("debug", _SE(jsbConsoleDebug));

        consoleVal.toObject()->getProperty("info", &oldConsoleInfo);
        consoleVal.toObject()->defineFunction("info", _SE(jsbConsoleInfo));

        consoleVal.toObject()->getProperty("warn", &oldConsoleWarn);
        consoleVal.toObject()->defineFunction("warn", _SE(jsbConsoleWarn));

        consoleVal.toObject()->getProperty("error", &oldConsoleError);
        consoleVal.toObject()->defineFunction("error", _SE(jsbConsoleError));

        consoleVal.toObject()->getProperty("assert", &oldConsoleAssert);
        consoleVal.toObject()->defineFunction("assert", _SE(jsbConsoleAssert));
    }

    _globalObj->setProperty("scriptEngineType", se::Value("V8"));

    _globalObj->defineFunction("log", seLogCallback);
    _globalObj->defineFunction("forceGC", seForceGC);

    _globalObj->getProperty(EXPOSE_GC, &_gcFuncValue);
    if (_gcFuncValue.isObject() && _gcFuncValue.toObject()->isFunction()) {
        _gcFunc = _gcFuncValue.toObject();
    } else {
        _gcFunc = nullptr;
    }

    __jsb_CCPrivateData_class = Class::create("__PrivateData", _globalObj, nullptr, nullptr);
    __jsb_CCPrivateData_class->defineFinalizeFunction(privateDataFinalize);
    __jsb_CCPrivateData_class->setCreateProto(false);
    __jsb_CCPrivateData_class->install();

    _isValid = true;

    for (const auto &hook : _afterInitHookArray) {
        hook();
    }
    _afterInitHookArray.clear();

    return _isValid;
}

void ScriptEngine::cleanup() {
    if (!_isValid) {
        return;
    }

    SE_LOGD("ScriptEngine::cleanup begin ...\n");
    _isInCleanup = true;

    {
        AutoHandleScope hs;
        for (const auto &hook : _beforeCleanupHookArray) {
            hook();
        }
        _beforeCleanupHookArray.clear();

        SAFE_DEC_REF(_globalObj);
        Object::cleanup();
        Class::cleanup();
        garbageCollect();

        oldConsoleLog.setUndefined();
        oldConsoleDebug.setUndefined();
        oldConsoleInfo.setUndefined();
        oldConsoleWarn.setUndefined();
        oldConsoleError.setUndefined();
        oldConsoleAssert.setUndefined();

    #if SE_ENABLE_INSPECTOR

        if (_env != nullptr) {
            _env->inspector_agent()->Disconnect();
            _env->inspector_agent()->Stop();
        }

        if (_isolateData != nullptr) {
            node::FreeIsolateData(_isolateData);
            _isolateData = nullptr;
        }

        if (_env != nullptr) {
            _env->CleanupHandles();
            node::FreeEnvironment(_env);
            _env = nullptr;
        }
    #endif

        _context.Get(_isolate)->Exit();
        _context.Reset();
        _isolate->Exit();
    }
    _isolate->Dispose();

    _isolate   = nullptr;
    _globalObj = nullptr;
    _isValid   = false;

    _registerCallbackArray.clear();

    for (const auto &hook : _afterCleanupHookArray) {
        hook();
    }
    _afterCleanupHookArray.clear();

    _isInCleanup = false;
    NativePtrToObjectMap::destroy();
    NonRefNativePtrCreatedByCtorMap::destroy();
    _gcFunc = nullptr;
    SE_LOGD("ScriptEngine::cleanup end ...\n");
}

Object *ScriptEngine::getGlobalObject() const {
    return _globalObj;
}

void ScriptEngine::addBeforeInitHook(const std::function<void()> &hook) {
    _beforeInitHookArray.push_back(hook);
}

void ScriptEngine::addAfterInitHook(const std::function<void()> &hook) {
    _afterInitHookArray.push_back(hook);
}

void ScriptEngine::addBeforeCleanupHook(const std::function<void()> &hook) {
    _beforeCleanupHookArray.push_back(hook);
}

void ScriptEngine::addAfterCleanupHook(const std::function<void()> &hook) {
    _afterCleanupHookArray.push_back(hook);
}

void ScriptEngine::addRegisterCallback(RegisterCallback cb) {
    assert(std::find(_registerCallbackArray.begin(), _registerCallbackArray.end(), cb) == _registerCallbackArray.end());
    _registerCallbackArray.push_back(cb);
}

void ScriptEngine::addPermanentRegisterCallback(RegisterCallback cb) {
    if (std::find(_permRegisterCallbackArray.begin(), _permRegisterCallbackArray.end(), cb) == _permRegisterCallbackArray.end()) {
        _permRegisterCallbackArray.push_back(cb);
    }
}

bool ScriptEngine::start() {
    if (!init()) {
        return false;
    }

    se::AutoHandleScope hs;

    // debugger
    if (isDebuggerEnabled()) {
    #if SE_ENABLE_INSPECTOR
        // V8 inspector stuff, most code are taken from NodeJS.
        _isolateData = node::CreateIsolateData(_isolate, uv_default_loop());
        _env         = node::CreateEnvironment(_isolateData, _context.Get(_isolate), 0, nullptr, 0, nullptr);

        node::DebugOptions options;
        options.set_wait_for_connect(_isWaitForConnect); // the program will be hung up until debug attach if _isWaitForConnect = true
        options.set_inspector_enabled(true);
        options.set_port(static_cast<int>(_debuggerServerPort));
        options.set_host_name(_debuggerServerAddr);
        bool ok = _env->inspector_agent()->Start(gSharedV8->platform, "", options);
        assert(ok);
    #endif
    }
    //
    bool ok    = false;
    _startTime = std::chrono::steady_clock::now();

    for (auto cb : _permRegisterCallbackArray) {
        ok = cb(_globalObj);
        assert(ok);
        if (!ok) {
            break;
        }
    }

    for (auto cb : _registerCallbackArray) {
        ok = cb(_globalObj);
        assert(ok);
        if (!ok) {
            break;
        }
    }

    // After ScriptEngine is started, _registerCallbackArray isn't needed. Therefore, clear it here.
    _registerCallbackArray.clear();

    return ok;
}

void ScriptEngine::garbageCollect() {
    int objSize = __objectMap ? static_cast<int>(__objectMap->size()) : -1;
    SE_LOGD("GC begin ..., (js->native map) size: %d, all objects: %d\n", (int)NativePtrToObjectMap::size(), objSize);

    if (_gcFunc == nullptr) {
        const double kLongIdlePauseInSeconds = 1.0;
        _isolate->ContextDisposedNotification();
        _isolate->IdleNotificationDeadline(gSharedV8->platform->MonotonicallyIncreasingTime() + kLongIdlePauseInSeconds);
        // By sending a low memory notifications, we will try hard to collect all
        // garbage and will therefore also invoke all weak callbacks of actually
        // unreachable persistent handles.
        _isolate->LowMemoryNotification();
    } else {
        _gcFunc->call({}, nullptr);
    }
    objSize = __objectMap ? static_cast<int>(__objectMap->size()) : -1;

    SE_LOGD("GC end ..., (js->native map) size: %d, all objects: %d\n", (int)NativePtrToObjectMap::size(), objSize);
}

bool ScriptEngine::isGarbageCollecting() const {
    return _isGarbageCollecting;
}

void ScriptEngine::_setGarbageCollecting(bool isGarbageCollecting) { //NOLINT(readability-identifier-naming)
    _isGarbageCollecting = isGarbageCollecting;
}

bool ScriptEngine::isValid() const {
    return _isValid;
}

bool ScriptEngine::evalString(const char *script, ssize_t length /* = -1 */, Value *ret /* = nullptr */, const char *fileName /* = nullptr */) {
    if (_engineThreadId != std::this_thread::get_id()) {
        // `evalString` should run in main thread
        assert(false);
        return false;
    }

    assert(script != nullptr);
    if (length < 0) {
        length = static_cast<ssize_t>(strlen(script));
    }

    if (fileName == nullptr) {
        fileName = "(no filename)";
    }

    // Fix the source url is too long displayed in Chrome debugger.
    std::string              sourceUrl  = fileName;
    static const std::string PREFIX_KEY = "/temp/quick-scripts/";
    size_t                   prefixPos  = sourceUrl.find(PREFIX_KEY);
    if (prefixPos != std::string::npos) {
        sourceUrl = sourceUrl.substr(prefixPos + PREFIX_KEY.length());
    }

    #if CC_PLATFORM == CC_PLATFORM_MAC_OSX
    if (strncmp("(no filename)", sourceUrl.c_str(), sizeof("(no filename)")) != 0) {
        sourceUrl = cc::FileUtils::getInstance()->fullPathForFilename(sourceUrl);
    }
    #endif

    // It is needed, or will crash if invoked from non C++ context, such as invoked from objective-c context(for example, handler of UIKit).
    v8::HandleScope handleScope(_isolate);

    std::string                scriptStr(script, length);
    v8::MaybeLocal<v8::String> source = v8::String::NewFromUtf8(_isolate, scriptStr.c_str(), v8::NewStringType::kNormal);
    if (source.IsEmpty()) {
        return false;
    }

    v8::MaybeLocal<v8::String> originStr = v8::String::NewFromUtf8(_isolate, sourceUrl.c_str(), v8::NewStringType::kNormal);
    if (originStr.IsEmpty()) {
        return false;
    }

    v8::ScriptOrigin           origin(originStr.ToLocalChecked());
    v8::MaybeLocal<v8::Script> maybeScript = v8::Script::Compile(_context.Get(_isolate), source.ToLocalChecked(), &origin);

    bool success = false;

    if (!maybeScript.IsEmpty()) {
        v8::TryCatch block(_isolate);

        v8::Local<v8::Script>     v8Script    = maybeScript.ToLocalChecked();
        v8::MaybeLocal<v8::Value> maybeResult = v8Script->Run(_context.Get(_isolate));

        if (!maybeResult.IsEmpty()) {
            v8::Local<v8::Value> result = maybeResult.ToLocalChecked();

            if (!result->IsUndefined() && ret != nullptr) {
                internal::jsToSeValue(_isolate, result, ret);
            }

            success = true;
        }

        if (block.HasCaught()) {
            v8::Local<v8::Message> message = block.Message();
            SE_LOGE("ScriptEngine::evalString catch exception:\n");
            onMessageCallback(message, v8::Undefined(_isolate));
        }
    }

    if (!success) {
        SE_LOGE("ScriptEngine::evalString script %s, failed!\n", fileName);
    }
    return success;
}

std::string ScriptEngine::getCurrentStackTrace() {
    if (!_isValid) {
        return std::string();
    }

    v8::HandleScope           hs(_isolate);
    v8::Local<v8::StackTrace> stack = v8::StackTrace::CurrentStackTrace(_isolate, JSB_STACK_FRAME_LIMIT, v8::StackTrace::kOverview);
    return stackTraceToString(stack);
}

void ScriptEngine::setFileOperationDelegate(const FileOperationDelegate &delegate) {
    _fileOperationDelegate = delegate;
}

const ScriptEngine::FileOperationDelegate &ScriptEngine::getFileOperationDelegate() const {
    return _fileOperationDelegate;
}

bool ScriptEngine::saveByteCodeToFile(const std::string &path, const std::string &pathBc) {
    bool  success = false;
    auto *fu      = cc::FileUtils::getInstance();

    if (pathBc.length() > 3 && pathBc.substr(pathBc.length() - 3) != ".bc") {
        SE_LOGE("ScriptEngine::generateByteCode bytecode file path should endwith \".bc\"\n");
        ;
        return false;
    }

    if (fu->isFileExist(pathBc)) {
        SE_LOGE("ScriptEngine::generateByteCode file already exists, it will be rewrite!\n");
    }

    //create directory for .bc file
    {
        auto lastSep = static_cast<int>(pathBc.size()) - 1;
        while (lastSep >= 0 && pathBc[lastSep] != '/') {
            lastSep -= 1;
        }

        if (lastSep == 0) {
            SE_LOGE("ScriptEngine::generateByteCode no directory component found in path %s\n", path.c_str());
            return false;
        }
        std::string pathBcDir = pathBc.substr(0, lastSep);
        success               = fu->createDirectory(pathBcDir);
        if (!success) {
            SE_LOGE("ScriptEngine::generateByteCode failed to create bytecode for %s\n", path.c_str());
            return success;
        }
    }

    // load script file
    std::string           scriptBuffer = _fileOperationDelegate.onGetStringFromFile(path);
    v8::Local<v8::String> code         = v8::String::NewFromUtf8(_isolate, scriptBuffer.c_str(), v8::NewStringType::kNormal, static_cast<int>(scriptBuffer.length())).ToLocalChecked();
    v8::Local<v8::Value>  scriptPath   = v8::String::NewFromUtf8(_isolate, path.data(), v8::NewStringType::kNormal).ToLocalChecked();
    // create unbound script
    v8::ScriptOrigin             origin(scriptPath);
    v8::ScriptCompiler::Source   source(code, origin);
    v8::Local<v8::Context>       parsingContext = v8::Local<v8::Context>::New(_isolate, _context);
    v8::Context::Scope           parsingScope(parsingContext);
    v8::TryCatch                 tryCatch(_isolate);
    v8::Local<v8::UnboundScript> v8Script = v8::ScriptCompiler::CompileUnboundScript(_isolate, &source, v8::ScriptCompiler::kEagerCompile)
                                                .ToLocalChecked();
    // create CachedData
    v8::ScriptCompiler::CachedData *cd = v8::ScriptCompiler::CreateCodeCache(v8Script);
    // save to file
    cc::Data writeData;
    writeData.copy(cd->data, cd->length);
    success = fu->writeDataToFile(writeData, pathBc);
    if (!success) {
        SE_LOGE("ScriptEngine::generateByteCode write %s\n", pathBc.c_str());
    }
    return success;
}

bool ScriptEngine::runByteCodeFile(const std::string &pathBc, Value *ret /* = nullptr */) {
    auto *fu = cc::FileUtils::getInstance();

    cc::Data cachedData;
    fu->getContents(pathBc, &cachedData);

    // read origin source file length from .bc file
    uint8_t *p        = cachedData.getBytes() + 8;
    int      filesize = p[0] + (p[1] << 8) + (p[2] << 16) + (p[3] << 24);

    {
        // fix bytecode
        v8::HandleScope                 scope(_isolate);
        v8::Local<v8::String>           dummyBytecodeSource = v8::String::NewFromUtf8(_isolate, "\" \"", v8::NewStringType::kNormal).ToLocalChecked();
        v8::ScriptCompiler::Source      dummySource(dummyBytecodeSource);
        v8::Local<v8::UnboundScript>    dummyFunction = v8::ScriptCompiler::CompileUnboundScript(_isolate, &dummySource, v8::ScriptCompiler::kEagerCompile).ToLocalChecked();
        v8::ScriptCompiler::CachedData *dummyData     = v8::ScriptCompiler::CreateCodeCache(dummyFunction);
        memcpy(p + 4, dummyData->data + 12, 4);
        // delete dummyData; //NOTE: managed by v8
    }

    // setup ScriptOrigin
    v8::Local<v8::Value>   scriptPath  = v8::String::NewFromUtf8(_isolate, pathBc.data(), v8::NewStringType::kNormal).ToLocalChecked();
    v8::Local<v8::Integer> offset      = v8::Int32::New(_isolate, 0);
    v8::Local<v8::Integer> column      = v8::Int32::New(_isolate, 0);
    v8::Local<v8::Boolean> crossOrigin = v8::Boolean::New(_isolate, true);
    ;
    v8::ScriptOrigin origin(scriptPath, offset, column, crossOrigin);

    // restore CacheData
    auto *                v8CacheData = new v8::ScriptCompiler::CachedData(cachedData.getBytes(), static_cast<int>(cachedData.getSize()));
    v8::Local<v8::String> dummyCode;

    // generate dummy code
    if (filesize > 0) {
        std::vector<char> codeBuffer;
        codeBuffer.resize(filesize + 1);
        std::fill(codeBuffer.begin(), codeBuffer.end(), ' ');
        codeBuffer[0]            = '\"';
        codeBuffer[filesize - 1] = '\"';
        codeBuffer[filesize]     = '\0';
        dummyCode                = v8::String::NewFromUtf8(_isolate, codeBuffer.data(), v8::NewStringType::kNormal, filesize).ToLocalChecked();

        assert(dummyCode->Length() == filesize);
    }

    v8::ScriptCompiler::Source source(dummyCode, origin, v8CacheData);

    if (source.GetCachedData() == nullptr) {
        SE_LOGE("ScriptEngine::runByteCodeFile can not load cacheData for %s", pathBc.c_str());
        return false;
    }

    v8::TryCatch                 tryCatch(_isolate);
    v8::Local<v8::UnboundScript> v8Script = v8::ScriptCompiler::CompileUnboundScript(_isolate, &source, v8::ScriptCompiler::kConsumeCodeCache)
                                                .ToLocalChecked();

    if (v8Script.IsEmpty()) {
        SE_LOGE("ScriptEngine::runByteCodeFile can not compile %s!\n", pathBc.c_str());
        return false;
    }

    if (source.GetCachedData()->rejected) {
        SE_LOGE("ScriptEngine::runByteCodeFile cache rejected %s!\n", pathBc.c_str());
        return false;
    }

    v8::Local<v8::Script>     runnableScript = v8Script->BindToCurrentContext();
    v8::MaybeLocal<v8::Value> result         = runnableScript->Run(_context.Get(_isolate));

    if (result.IsEmpty()) {
        SE_LOGE("ScriptEngine::runByteCodeFile script %s, failed!\n", pathBc.c_str());
        return false;
    }

    if (!result.ToLocalChecked()->IsUndefined() && ret != nullptr) {
        internal::jsToSeValue(_isolate, result.ToLocalChecked(), ret);
    }

    SE_LOGE("ScriptEngine::runByteCodeFile success %s!\n", pathBc.c_str());

    return true;
}

bool ScriptEngine::runScript(const std::string &path, Value *ret /* = nullptr */) {
    assert(!path.empty());
    assert(_fileOperationDelegate.isValid());

    if (!cc::FileUtils::getInstance()->isFileExist(path)) {
        std::stringstream ss;
        ss << "throw new Error(\"Failed to require file '"
           << path << "', not found!\");";
        evalString(ss.str().c_str());
        return false;
    }

    if (path.length() > 3 && path.substr(path.length() - 3) == ".bc") {
        return runByteCodeFile(path, ret);
    }

    std::string scriptBuffer = _fileOperationDelegate.onGetStringFromFile(path);

    if (!scriptBuffer.empty()) {
        return evalString(scriptBuffer.c_str(), static_cast<ssize_t>(scriptBuffer.length()), ret, path.c_str());
    }

    SE_LOGE("ScriptEngine::runScript script %s, buffer is empty!\n", path.c_str());
    return false;
}

bool ScriptEngine::import(const std::string &specifier_, std::string *parentURL) {
    this->_environment->import(
        v8::String::NewFromUtf8(_isolate, specifier_.data()).ToLocalChecked(),
        !parentURL ? v8::MaybeLocal<v8::String>{} : v8::String::NewFromUtf8(_isolate, specifier_.data()).ToLocalChecked());
    return true;
}

void ScriptEngine::runInInternal(const std::string &path_) {
    this->_environment->runInInternal(path_);
}

void ScriptEngine::clearException() {
    //IDEA:
}

void ScriptEngine::throwException(const std::string &errorMessage) {
    v8::HandleScope       scope(_isolate);
    v8::Local<v8::String> message = v8::String::NewFromUtf8(_isolate, errorMessage.data()).ToLocalChecked();
    v8::Local<v8::Value>  error   = v8::Exception::Error(message);
    _isolate->ThrowException(error);
}

void ScriptEngine::setExceptionCallback(const ExceptionCallback &cb) {
    _nativeExceptionCallback = cb;
}

void ScriptEngine::setJSExceptionCallback(const ExceptionCallback &cb) {
    _jsExceptionCallback = cb;
}

v8::Local<v8::Context> ScriptEngine::_getContext() const { //NOLINT(readability-identifier-naming)
    return _context.Get(_isolate);
}

void ScriptEngine::enableDebugger(const std::string &serverAddr, uint32_t port, bool isWait) {
    _debuggerServerAddr = serverAddr;
    _debuggerServerPort = port;
    _isWaitForConnect   = isWait;
}

bool ScriptEngine::isDebuggerEnabled() const {
    return !_debuggerServerAddr.empty() && _debuggerServerPort > 0;
}

void ScriptEngine::mainLoopUpdate() {
    // empty implementation
}

} // namespace se

#endif // #if SCRIPT_ENGINE_TYPE == SCRIPT_ENGINE_V8
