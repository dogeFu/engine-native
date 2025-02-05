/****************************************************************************
 Copyright (c) 2020-2021 Huawei Technologies Co., Ltd.

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

#pragma once

#include <array>

#include "gfx-base/GFXBuffer.h"
#include "gfx-base/GFXInputAssembler.h"
#include "pipeline/RenderPipeline.h"
#include "frame-graph/FrameGraph.h"
#include "frame-graph/Handle.h"
#include <unordered_map>

namespace cc {
namespace pipeline {
struct UBOGlobal;
struct UBOCamera;
struct UBOShadow;

struct CC_DLL DeferredRenderData {
    gfx::TextureList  gbufferRenderTargets;
    gfx::Framebuffer *gbufferFrameBuffer   = nullptr;
    gfx::Framebuffer *lightingFrameBuff    = nullptr;
    gfx::Texture *    lightingRenderTarget = nullptr;
    gfx::Texture *    depthTex             = nullptr;
};

enum class DeferredInsertPoint {
    IP_GBUFFER = 100,
    IP_LIGHTING = 200,
    IP_TRANSPARENT = 220,
    IP_SSPR = 300,
    IP_POSTPROCESS = 400,
    IP_INVALID
};

class CC_DLL DeferredPipeline : public RenderPipeline {
public:
    DeferredPipeline()           = default;
    ~DeferredPipeline() override = default;

    bool initialize(const RenderPipelineInfo &info) override;
    void destroy() override;
    bool activate() override;
    void render(const vector<scene::Camera *> &cameras) override;
    void resize(uint width, uint height) override;

    inline gfx::Buffer *          getLightsUBO() const { return _lightsUBO; }
    inline const LightList &      getValidLights() const { return _validLights; }
    inline const gfx::BufferList &getLightBuffers() const { return _lightBuffers; }
    inline const UintList &       getLightIndexOffsets() const { return _lightIndexOffsets; }
    inline const UintList &       getLightIndices() const { return _lightIndices; }
    gfx::Rect                     getRenderArea(scene::Camera *camera, bool onScreen);
    void                          updateQuadVertexData(const gfx::Rect &renderArea, gfx::Buffer *buffer);
    void                          genQuadVertexData(gfx::SurfaceTransform surfaceTransform, const gfx::Rect &renderArea, float *data);

    framegraph::FrameGraph &getFrameGraph() { return _fg; }
    gfx::Color getClearcolor(scene::Camera *camera);
    void prepareFrameGraph();
    void initFrameGraphExternalTexture();
    void destroyFrameGraphExternalTexture();
    uint getWidth() const { return _width; }
    uint getHeight() const { return _height; }
    scene::Camera *getFrameGraphCamera() const { return _frameGraphCamera; }
    gfx::InputAssembler *getIAByRenderArea(const gfx::Rect &rect);

private:
    bool activeRenderer();
    bool createQuadInputAssembler(gfx::Buffer *quadIB, gfx::Buffer **quadVB, gfx::InputAssembler **quadIA);
    void destroyQuadInputAssembler();
    void destroyDeferredData();
    void generateDeferredRenderData();

    gfx::Buffer *                           _lightsUBO = nullptr;
    LightList                               _validLights;
    gfx::BufferList                         _lightBuffers;
    UintList                                _lightIndexOffsets;
    UintList                                _lightIndices;
    map<gfx::ClearFlags, gfx::RenderPass *> _renderPasses;
    gfx::Rect                               _lastUsedRenderArea;

    gfx::Buffer *                                   _quadIB = nullptr;
    std::vector<gfx::Buffer *>                      _quadVB;
    std::unordered_map<uint, gfx::InputAssembler *> _quadIA;

    uint             _width = 0;
    uint             _height = 0;

    framegraph::FrameGraph _fg;
    scene::Camera *_frameGraphCamera = nullptr;

public:
    // deferred resource name declear
    static framegraph::StringHandle fgStrHandleGbufferTexture[4];        // deal as external resource
    static framegraph::StringHandle fgStrHandleDepthTexture;             // deal as external resource
    static framegraph::StringHandle fgStrHandleDepthTexturePost;         // deal as external resource
    static framegraph::StringHandle fgStrHandleLightingOutTexture;
    static framegraph::StringHandle fgStrHandleBackBufferTexture;        // buffer get from swapchain, used for queuepresent

    // deferred pass name declear
    static framegraph::StringHandle fgStrHandleGbufferPass;
    static framegraph::StringHandle fgStrHandleLightingPass;
    static framegraph::StringHandle fgStrHandleTransparentPass;
    static framegraph::StringHandle fgStrHandleSsprPass;
    static framegraph::StringHandle fgStrHandlePostprocessPass;

    // external resources of framegraph
    static const uint GBUFFER_COUNT = 4;

    // In deferred pipeline, all passes use the same global descriptorset.
    // In vulkan spec, a descriptorset cannot update after first use, so we should bind all bindings before any pass executing
    // So gbuffer should create gfx texture and binding to global descriptorset in DeferredPipeline::activeRenderer
    // gbuffer's depth is not the same as post-process
    framegraph::Texture *fgTextureGbuffer[GBUFFER_COUNT] = {nullptr};
    framegraph::Texture *fgTextureDepth = nullptr;
    framegraph::Texture fgTextureDepthPost;
    framegraph::Texture fgTextureBackBuffer;
};

} // namespace pipeline
} // namespace cc
