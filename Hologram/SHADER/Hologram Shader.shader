// Made with Amplify Shader Editor v1.9.8
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Knife/Hologram Shader Unlit"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[HDR]_MainColor("Main Color", Color) = (0.620945,1.420074,3.953349,0.05098039)
		[NoScaleOffset]_Line1("Line 1", 2D) = "white" {}
		_Line1Speed("Line 1 Speed", Float) = -3.57
		_Line1Frequency("Line 1 Frequency", Float) = 100
		_Line1Hardness("Line 1 Hardness", Float) = 1.45
		_Line1InvertedThickness("Line 1 Inverted Thickness", Range( 0 , 1)) = 0
		_Line1Alpha("Line 1 Alpha", Float) = 0.15
		[NoScaleOffset]_Line2("Line 2", 2D) = "white" {}
		_Line2Speed("Line 2 Speed", Float) = -1
		_Line2Frequency("Line 2 Frequency", Float) = 1
		_Line2Hardness("Line 2 Hardness", Float) = 2
		_Line2InvertedThickness("Line 2 Inverted Thickness", Range( 0 , 1)) = 0.255
		_Line2Alpha("Line 2 Alpha", Float) = 0.1
		[NoScaleOffset]_LineGlitch("Line Glitch", 2D) = "white" {}
		_LineGlitchOffset("Line Glitch Offset", Vector) = (0.03,0,0,0)
		_RandomGlitchOffset("Random Glitch Offset", Vector) = (-0.5,0,0,0)
		_RandomGlitchAmount("Random Glitch Amount", Range( 0 , 1)) = 0.089
		_ColorGlitchAffect("Color Glitch Affect", Range( 0 , 1)) = 0.5
		_LineGlitchSpeed("Line Glitch Speed", Float) = -0.26
		_LineGlitchFrequency("Line Glitch Frequency", Float) = 0.2
		_LineGlitchHardness("Line Glitch Hardness", Float) = 5
		_LineGlitchInvertedThickness("Line Glitch Inverted Thickness", Range( 0 , 1)) = 0.825
		_FresnelScale("Fresnel Scale", Float) = 1
		_FresnelPower("Fresnel Power", Float) = 2
		_FresnelAlphaScale("Fresnel Alpha Scale", Float) = 1
		_FresnelAlphaPower("Fresnel Alpha Power", Float) = 2
		_NormalMap("NormalMap", 2D) = "bump" {}
		_NormalScale("NormalScale", Float) = 0
		_SoftIntersection2Distance("Soft Intersection 2 Distance", Float) = 0
		_SoftIntersection1Distance("Soft Intersection 1 Distance", Float) = 0
		_NormalAffect("NormalAffect", Range( 0 , 1)) = 0
		_MaskCenter("Mask Center", Vector) = (0,0,0,0)
		_SoftIntersection2Intensity("Soft Intersection 2 Intensity", Float) = 1
		_SoftIntersection2Affect("Soft Intersection 2 Affect", Range( 0 , 1)) = 1
		_SoftIntersection1Intensity("Soft Intersection 1 Intensity", Float) = 1
		_SoftIntersection1Affect("Soft Intersection 1 Affect", Range( 0 , 1)) = 1
		_MaskSize("Mask Size", Vector) = (0,0,0,0)
		_RandomGlitchConstant("Random Glitch Constant", Range( 0 , 1)) = 0
		_DissolveScale("Dissolve Scale", Vector) = (0.1,1.01,5,0)
		_GrainScale("Grain Scale", Vector) = (50,50,50,0)
		_GrainAffect("Grain Affect", Range( 0 , 1)) = 1
		[Toggle(_COLORGLITCHFEATURE_ON)] _ColorGlitchFeature("Color Glitch Feature", Float) = 0
		[Toggle(_GRAINFEATURE_ON)] _GrainFeature("Grain Feature", Float) = 0
		_GrainValues("Grain Values", Vector) = (0,1,0,0)
		_RandomGlitchTiling("Random Glitch Tiling", Float) = 2.83
		_MaskFalloff("Mask Falloff", Float) = 0
		[Toggle(_LINE2FEATURE_ON)] _Line2Feature("Line 2 Feature", Float) = 0
		[Toggle(_FRESNELFEATURE_ON)] _FresnelFeature("Fresnel Feature", Float) = 0
		[Toggle(_LINEGLITCHFEATURE_ON)] _LineGlitchFeature("Line Glitch Feature", Float) = 0
		[Toggle(_RANDOMGLITCHFEATURE_ON)] _RandomGlitchFeature("Random Glitch Feature", Float) = 0
		[KeywordEnum(Off,Alpha,Color)] _SoftIntersection2Feature("Soft Intersection 2 Feature", Float) = 0
		[KeywordEnum(Off,Alpha,Color)] _SoftIntersection1Feature("Soft Intersection 1 Feature", Float) = 0
		[Toggle(_LINE1FEATURE_ON)] _Line1Feature("Line 1 Feature", Float) = 0
		[Toggle(_LINEBOTHFEATURE_ON)] _LineBothFeature("Line Both Feature", Float) = 0
		_DissolveHide("Dissolve Hide", Range( -1 , 1)) = -1
		[Toggle(_DISSOLVEFEATURE_ON)] _DissolveFeature("Dissolve Feature", Float) = 0
		[Toggle(_MASKFEATURE_ON)] _MaskFeature("Mask Feature", Float) = 0
		[Toggle(_NORMALMAPFEATURE_ON)] _NormalMapFeature("Normal Map Feature", Float) = 0
		[KeywordEnum(X,Y,Z)] _PositionFeature("Position Feature", Float) = 1
		[KeywordEnum(World,Local,Custom)] _PositionSpaceFeature("Position Space Feature", Float) = 0
		_PositionDirection("Position Direction", Float) = 1
		_RandomOffset("Random Offset", Float) = 0
		_AlphaMask("Alpha Mask", 2D) = "white" {}
		_AlphaMaskAffect("Alpha Mask Affect", Range( 0 , 1)) = 0.5
		[Toggle(_ALPHAMASKFEATURE_ON)] _AlphaMaskFeature("Alpha Mask Feature", Float) = 0
		[Toggle]_ZWrite("ZWrite", Float) = 0
		[Enum(UnityEngine.Rendering.CullMode)]_CullMode("Cull Mode", Float) = 2
		[Toggle(_MASKLOCALFEATURE_ON)] _MaskLocalFeature("Mask Local Feature", Float) = 0
		_MaskInversion("Mask Inversion", Range( 0 , 1)) = 0
		_Voxelization("Voxelization", Float) = 100
		_VoxelizationAffect("Voxelization Affect", Range( 0 , 1)) = 1
		[Toggle(_VOXELIZATIONFEATURE_ON)] _VoxelizationFeature("Voxelization Feature", Float) = 0
		_Alpha("Alpha", Range( 0 , 1)) = 1
		[HideInInspector] _texcoord( "", 2D ) = "white" {}


		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25

		[HideInInspector] _QueueOffset("_QueueOffset", Float) = 0
        [HideInInspector] _QueueControl("_QueueControl", Float) = -1

        [HideInInspector][NoScaleOffset] unity_Lightmaps("unity_Lightmaps", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset] unity_LightmapsInd("unity_LightmapsInd", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset] unity_ShadowMasks("unity_ShadowMasks", 2DArray) = "" {}

		//[HideInInspector][ToggleUI] _AddPrecomputedVelocity("Add Precomputed Velocity", Float) = 1
		[HideInInspector][ToggleOff] _ReceiveShadows("Receive Shadows", Float) = 1.0
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="Transparent" "UniversalMaterialType"="Unlit" }

		Cull [_CullMode]
		AlphaToMask Off

		

		HLSLINCLUDE
		#pragma target 4.5
		#pragma prefer_hlslcc gles
		// ensure rendering platforms toggle list is visible

		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Common.hlsl"
		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Filtering.hlsl"

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}

		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS
		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForwardOnly" }

			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite [_ZWrite]
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			

			HLSLPROGRAM

			#pragma instancing_options renderinglayer
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_VERSION 19800
			#define ASE_SRP_VERSION 170003
			#define REQUIRE_DEPTH_TEXTURE 1


			#pragma multi_compile_fragment _ _SCREEN_SPACE_OCCLUSION
			#pragma multi_compile_fragment _ _DBUFFER_MRT1 _DBUFFER_MRT2 _DBUFFER_MRT3
			#pragma multi_compile_fragment _ _GBUFFER_NORMALS_OCT

			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
            #pragma multi_compile _ LIGHTMAP_ON
            #pragma multi_compile _ DYNAMICLIGHTMAP_ON
			#pragma multi_compile_fragment _ DEBUG_DISPLAY

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_UNLIT

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Input.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRenderingKeywords.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRendering.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DBuffer.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Debug/Debugging3D.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/SurfaceData.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_WORLD_VIEW_DIR
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_POSITION
			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature _VOXELIZATIONFEATURE_ON
			#pragma shader_feature _LINEGLITCHFEATURE_ON
			#pragma shader_feature _POSITIONSPACEFEATURE_WORLD _POSITIONSPACEFEATURE_LOCAL _POSITIONSPACEFEATURE_CUSTOM
			#pragma shader_feature _POSITIONFEATURE_X _POSITIONFEATURE_Y _POSITIONFEATURE_Z
			#pragma multi_compile_instancing
			#pragma shader_feature _RANDOMGLITCHFEATURE_ON
			#pragma shader_feature _COLORGLITCHFEATURE_ON
			#pragma shader_feature _FRESNELFEATURE_ON
			#pragma shader_feature _NORMALMAPFEATURE_ON
			#pragma shader_feature _LINEBOTHFEATURE_ON
			#pragma shader_feature _LINE2FEATURE_ON
			#pragma shader_feature _LINE1FEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION1FEATURE_OFF _SOFTINTERSECTION1FEATURE_ALPHA _SOFTINTERSECTION1FEATURE_COLOR
			#pragma shader_feature _GRAINFEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION2FEATURE_OFF _SOFTINTERSECTION2FEATURE_ALPHA _SOFTINTERSECTION2FEATURE_COLOR
			#pragma shader_feature _DISSOLVEFEATURE_ON
			#pragma shader_feature _MASKFEATURE_ON
			#pragma shader_feature _MASKLOCALFEATURE_ON
			#pragma shader_feature _ALPHAMASKFEATURE_ON


			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE) && (SHADER_TARGET >= 45)
				#define ASE_SV_DEPTH SV_DepthLessEqual
				#define ASE_SV_POSITION_QUALIFIERS linear noperspective centroid
			#else
				#define ASE_SV_DEPTH SV_Depth
				#define ASE_SV_POSITION_QUALIFIERS
			#endif

			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				ASE_SV_POSITION_QUALIFIERS float4 positionCS : SV_POSITION;
				float4 clipPosV : TEXCOORD0;
				float3 positionWS : TEXCOORD1;
				#if defined(ASE_FOG) || defined(_ADDITIONAL_LIGHTS_VERTEX)
					half4 fogFactorAndVertexLight : TEXCOORD2;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					float4 shadowCoord : TEXCOORD3;
				#endif
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainColor;
			float3 _LineGlitchOffset;
			float3 _MaskSize;
			float3 _MaskCenter;
			float3 _DissolveScale;
			float3 _RandomGlitchOffset;
			float3 _GrainScale;
			float2 _GrainValues;
			float _CullMode;
			float _Line2Frequency;
			float _Line2Speed;
			float _Line2InvertedThickness;
			float _Line2Hardness;
			float _Line2Alpha;
			float _SoftIntersection1Distance;
			float _SoftIntersection1Intensity;
			float _GrainAffect;
			float _Line1Alpha;
			float _SoftIntersection2Distance;
			float _SoftIntersection2Intensity;
			float _SoftIntersection2Affect;
			float _DissolveHide;
			float _MaskFalloff;
			float _MaskInversion;
			float _SoftIntersection1Affect;
			float _Line1Hardness;
			float _Line1Speed;
			float _AlphaMaskAffect;
			float _ZWrite;
			float _PositionDirection;
			float _LineGlitchFrequency;
			float _LineGlitchSpeed;
			float _RandomOffset;
			float _LineGlitchInvertedThickness;
			float _LineGlitchHardness;
			float _RandomGlitchTiling;
			float _RandomGlitchConstant;
			float _Line1InvertedThickness;
			float _RandomGlitchAmount;
			float _VoxelizationAffect;
			float _ColorGlitchAffect;
			float _FresnelScale;
			float _FresnelPower;
			float _NormalScale;
			float _NormalAffect;
			float _FresnelAlphaScale;
			float _FresnelAlphaPower;
			float _Line1Frequency;
			float _Voxelization;
			float _Alpha;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _LineGlitch;
			sampler2D _NormalMap;
			sampler2D _Line1;
			sampler2D _Line2;
			sampler2D _AlphaMask;
			UNITY_INSTANCING_BUFFER_START(KnifeHologramShaderUnlit)
				UNITY_DEFINE_INSTANCED_PROP(float4x4, _CustomMatrix)
				UNITY_DEFINE_INSTANCED_PROP(float4, _NormalMap_ST)
				UNITY_DEFINE_INSTANCED_PROP(float4, _AlphaMask_ST)
			UNITY_INSTANCING_BUFFER_END(KnifeHologramShaderUnlit)


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float3 mod3D289( float3 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 mod3D289( float4 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 permute( float4 x ) { return mod3D289( ( x * 34.0 + 1.0 ) * x ); }
			float4 taylorInvSqrt( float4 r ) { return 1.79284291400159 - r * 0.85373472095314; }
			float snoise( float3 v )
			{
				const float2 C = float2( 1.0 / 6.0, 1.0 / 3.0 );
				float3 i = floor( v + dot( v, C.yyy ) );
				float3 x0 = v - i + dot( i, C.xxx );
				float3 g = step( x0.yzx, x0.xyz );
				float3 l = 1.0 - g;
				float3 i1 = min( g.xyz, l.zxy );
				float3 i2 = max( g.xyz, l.zxy );
				float3 x1 = x0 - i1 + C.xxx;
				float3 x2 = x0 - i2 + C.yyy;
				float3 x3 = x0 - 0.5;
				i = mod3D289( i);
				float4 p = permute( permute( permute( i.z + float4( 0.0, i1.z, i2.z, 1.0 ) ) + i.y + float4( 0.0, i1.y, i2.y, 1.0 ) ) + i.x + float4( 0.0, i1.x, i2.x, 1.0 ) );
				float4 j = p - 49.0 * floor( p / 49.0 );  // mod(p,7*7)
				float4 x_ = floor( j / 7.0 );
				float4 y_ = floor( j - 7.0 * x_ );  // mod(j,N)
				float4 x = ( x_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 y = ( y_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 h = 1.0 - abs( x ) - abs( y );
				float4 b0 = float4( x.xy, y.xy );
				float4 b1 = float4( x.zw, y.zw );
				float4 s0 = floor( b0 ) * 2.0 + 1.0;
				float4 s1 = floor( b1 ) * 2.0 + 1.0;
				float4 sh = -step( h, 0.0 );
				float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy;
				float4 a1 = b1.xzyw + s1.xzyw * sh.zzww;
				float3 g0 = float3( a0.xy, h.x );
				float3 g1 = float3( a0.zw, h.y );
				float3 g2 = float3( a1.xy, h.z );
				float3 g3 = float3( a1.zw, h.w );
				float4 norm = taylorInvSqrt( float4( dot( g0, g0 ), dot( g1, g1 ), dot( g2, g2 ), dot( g3, g3 ) ) );
				g0 *= norm.x;
				g1 *= norm.y;
				g2 *= norm.z;
				g3 *= norm.w;
				float4 m = max( 0.6 - float4( dot( x0, x0 ), dot( x1, x1 ), dot( x2, x2 ), dot( x3, x3 ) ), 0.0 );
				m = m* m;
				m = m* m;
				float4 px = float4( dot( x0, g0 ), dot( x1, g1 ), dot( x2, g2 ), dot( x3, g3 ) );
				return 42.0 * dot( m, px);
			}
			

			PackedVaryings VertexFunction( Attributes input  )
			{
				PackedVaryings output = (PackedVaryings)0;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float C_ZERO287 = 0.0;
				float3 temp_cast_0 = (C_ZERO287).xxx;
				float3 viewToObjDir94 = mul( UNITY_MATRIX_T_MV, float4( _LineGlitchOffset, 0 ) ).xyz;
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float3 ase_positionWS = TransformObjectToWorld( (input.positionOS).xyz );
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch350 = ase_positionWS.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch350 = ase_positionWS.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch350 = ase_positionWS.z;
				#else
				float staticSwitch350 = ase_positionWS.y;
				#endif
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch351 = input.positionOS.xyz.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch351 = input.positionOS.xyz.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch351 = input.positionOS.xyz.z;
				#else
				float staticSwitch351 = input.positionOS.xyz.y;
				#endif
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.positionOS.xyz , 0.0 ) ).xyz;
				float3 break379 = temp_output_377_0;
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch378 = break379.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch378 = break379.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch378 = break379.z;
				#else
				float staticSwitch378 = break379.y;
				#endif
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch352 = staticSwitch350;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch352 = staticSwitch351;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch352 = staticSwitch378;
				#else
				float staticSwitch352 = staticSwitch350;
				#endif
				float vertexToFrag497 = ( staticSwitch352 * _PositionDirection );
				float pos353 = vertexToFrag497;
				float mulTime3_g167 = _TimeParameters.x * _LineGlitchSpeed;
				float randomoffset385 = _RandomOffset;
				float2 temp_cast_3 = ((pos353*_LineGlitchFrequency + ( mulTime3_g167 + randomoffset385 ))).xx;
				float clampResult42_g167 = clamp( ( ( tex2Dlod( _LineGlitch, float4( temp_cast_3, 0, 0.0) ).r - _LineGlitchInvertedThickness ) * _LineGlitchHardness ) , 0.0 , 1.0 );
				#ifdef _LINEGLITCHFEATURE_ON
				float3 staticSwitch307 = ( ( viewToObjDir94 / ase_objectScale ) * clampResult42_g167 );
				#else
				float3 staticSwitch307 = temp_cast_0;
				#endif
				float3 lineglitch491 = staticSwitch307;
				float3 temp_cast_4 = (C_ZERO287).xxx;
				float3 viewToObjDir122 = mul( UNITY_MATRIX_T_MV, float4( _RandomGlitchOffset, 0 ) ).xyz;
				float mulTime149 = _TimeParameters.x * -2.3;
				float mulTime155 = _TimeParameters.x * -2.05;
				float2 appendResult153 = (float2((pos353*_RandomGlitchTiling + ( mulTime149 + randomoffset385 )) , ( randomoffset385 + mulTime155 )));
				float simplePerlin2D186 = snoise( appendResult153 );
				simplePerlin2D186 = simplePerlin2D186*0.5 + 0.5;
				float4 matrixToPos373 = float4( float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[0][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[1][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[2][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[3][3]);
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch365 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch365 = 0.0;
				#else
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#endif
				float mulTime139 = _TimeParameters.x * -5.74;
				float mulTime157 = _TimeParameters.x * -0.83;
				float2 appendResult158 = (float2((staticSwitch365*223.0 + ( mulTime139 + randomoffset385 )) , ( randomoffset385 + mulTime157 )));
				float simplePerlin2D187 = snoise( appendResult158 );
				simplePerlin2D187 = simplePerlin2D187*0.5 + 0.5;
				float clampResult148 = clamp( (-1.0 + (( simplePerlin2D187 + _RandomGlitchConstant ) - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float temp_output_197_0 = ( (-1.0 + (simplePerlin2D186 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) * clampResult148 );
				float2 break190 = appendResult153;
				float2 appendResult195 = (float2(( 20.0 * break190.x ) , break190.y));
				float simplePerlin2D188 = snoise( appendResult195 );
				simplePerlin2D188 = simplePerlin2D188*0.5 + 0.5;
				float clampResult192 = clamp( (-1.0 + (simplePerlin2D188 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult199 = lerp( 0.0 , clampResult192 , 2.0);
				#ifdef _RANDOMGLITCHFEATURE_ON
				float3 staticSwitch311 = ( ( viewToObjDir122 / ase_objectScale ) * ( temp_output_197_0 + ( temp_output_197_0 * lerpResult199 ) ) * _RandomGlitchAmount );
				#else
				float3 staticSwitch311 = temp_cast_4;
				#endif
				float3 randomglitch493 = staticSwitch311;
				float3 vertexoffset410 = ( lineglitch491 + randomglitch493 );
				float3 temp_output_512_0 = ( input.positionOS.xyz + vertexoffset410 );
				float3 lerpResult517 = lerp( temp_output_512_0 , ( round( ( temp_output_512_0 * _Voxelization ) ) / _Voxelization ) , _VoxelizationAffect);
				#ifdef _VOXELIZATIONFEATURE_ON
				float3 staticSwitch518 = lerpResult517;
				#else
				float3 staticSwitch518 = temp_output_512_0;
				#endif
				float3 ModifiedVertexPosition519 = staticSwitch518;
				
				float3 ase_normalWS = TransformObjectToWorldNormal(input.normalOS);
				output.ase_texcoord4.xyz = ase_normalWS;
				output.ase_texcoord4.w = vertexToFrag497;
				float3 vertexPos238 = ( input.positionOS.xyz + vertexoffset410 );
				float4 ase_positionCS238 = TransformObjectToHClip((vertexPos238).xyz);
				float4 screenPos238 = ComputeScreenPos(ase_positionCS238);
				output.ase_texcoord6 = screenPos238;
				float3 vertexPos417 = ( input.positionOS.xyz + vertexoffset410 );
				
				output.ase_texcoord5.xy = input.ase_texcoord.xy;
				output.ase_texcoord7 = input.positionOS;
				output.ase_color = input.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord5.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ModifiedVertexPosition519;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				VertexPositionInputs vertexInput = GetVertexPositionInputs( input.positionOS.xyz );
				
				#if defined(ASE_FOG) || defined(_ADDITIONAL_LIGHTS_VERTEX)
					output.fogFactorAndVertexLight = 0;
					#if defined(ASE_FOG) && !defined(_FOG_FRAGMENT)
						output.fogFactorAndVertexLight.x = ComputeFogFactor(vertexInput.positionCS.z);
					#endif
					#ifdef _ADDITIONAL_LIGHTS_VERTEX
						half3 vertexLight = VertexLighting( vertexInput.positionWS, normalInput.normalWS );
						output.fogFactorAndVertexLight.yzw = vertexLight;
					#endif
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					output.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				output.positionCS = vertexInput.positionCS;
				output.clipPosV = vertexInput.positionCS;
				output.positionWS = vertexInput.positionWS;
				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 positionOS : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.positionOS = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				output.ase_color = input.ase_color;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].positionOS, input[1].positionOS, input[2].positionOS, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].positionOS, input[1].positionOS, input[2].positionOS, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].positionOS, input[1].positionOS, input[2].positionOS, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].positionOS * bary.x + patch[1].positionOS * bary.y + patch[2].positionOS * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				output.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].positionOS.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			half4 frag ( PackedVaryings input
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						#ifdef _WRITE_RENDERING_LAYERS
						, out float4 outRenderingLayers : SV_Target1
						#endif
						 ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(input);

				#if defined(LOD_FADE_CROSSFADE)
					LODFadeCrossFade( input.positionCS );
				#endif

				float3 WorldPosition = input.positionWS;
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				float4 ClipPos = input.clipPosV;
				float4 ScreenPos = ComputeScreenPos( input.clipPosV );

				float2 NormalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(input.positionCS);

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = input.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float3 objToWorld161 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				float C_ZERO287 = 0.0;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch360 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch360 = 0.0;
				#else
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#endif
				float randomoffset385 = _RandomOffset;
				float mulTime165 = _TimeParameters.x * -15.0;
				float mulTime167 = _TimeParameters.x * -0.5;
				float2 appendResult168 = (float2((staticSwitch360*223.0 + ( randomoffset385 + mulTime165 )) , ( randomoffset385 + mulTime167 )));
				float simplePerlin2D176 = snoise( appendResult168 );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float clampResult175 = clamp( (-0.61 + (simplePerlin2D176 - 0.0) * (2.0 - -0.61) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult181 = lerp( 1.0 , clampResult175 , _ColorGlitchAffect);
				#ifdef _COLORGLITCHFEATURE_ON
				float staticSwitch279 = lerpResult181;
				#else
				float staticSwitch279 = 1.0;
				#endif
				float4 maincolor232 = _MainColor;
				float C_ONE288 = 1.0;
				float3 ase_viewDirWS = normalize( WorldViewDirection );
				float3 ase_normalWS = input.ase_texcoord4.xyz;
				float fresnelNdotV2 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode2 = ( 0.0 + _FresnelScale * pow( max( 1.0 - fresnelNdotV2 , 0.0001 ), _FresnelPower ) );
				float4 _NormalMap_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_NormalMap_ST);
				float2 uv_NormalMap = input.ase_texcoord5.xy * _NormalMap_ST_Instance.xy + _NormalMap_ST_Instance.zw;
				float3 unpack95 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalScale );
				unpack95.z = lerp( 1, unpack95.z, saturate(_NormalScale) );
				float3 ase_viewDirTS = normalize( WorldViewDirection );
				float dotResult101 = dot( unpack95 , ase_viewDirTS );
				float lerpResult106 = lerp( 1.0 , (0.0 + (dotResult101 - -1.0) * (1.0 - 0.0) / (1.0 - -1.0)) , _NormalAffect);
				#ifdef _NORMALMAPFEATURE_ON
				float staticSwitch341 = ( 1.0 - lerpResult106 );
				#else
				float staticSwitch341 = C_ZERO287;
				#endif
				float NormalAffect116 = staticSwitch341;
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch303 = ( fresnelNode2 + NormalAffect116 );
				#else
				float staticSwitch303 = C_ONE288;
				#endif
				float fresnelNdotV53 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode53 = ( 0.0 + _FresnelAlphaScale * pow( max( 1.0 - fresnelNdotV53 , 0.0001 ), _FresnelAlphaPower ) );
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch305 = fresnelNode53;
				#else
				float staticSwitch305 = C_ZERO287;
				#endif
				float clampResult57 = clamp( ( staticSwitch305 + NormalAffect116 ) , 0.0 , 1.0 );
				float4 fresnelcolor436 = ( staticSwitch303 * maincolor232 * clampResult57 );
				float4 temp_cast_0 = (C_ZERO287).xxxx;
				float vertexToFrag497 = input.ase_texcoord4.w;
				float pos353 = vertexToFrag497;
				float mulTime3_g169 = _TimeParameters.x * _Line1Speed;
				float2 temp_cast_1 = ((pos353*_Line1Frequency + ( mulTime3_g169 + randomoffset385 ))).xx;
				float clampResult42_g169 = clamp( ( ( tex2D( _Line1, temp_cast_1 ).r - _Line1InvertedThickness ) * _Line1Hardness ) , 0.0 , 1.0 );
				float temp_output_536_0 = clampResult42_g169;
				float3 temp_output_293_0 = (( maincolor232 * temp_output_536_0 )).rgb;
				float temp_output_61_0 = ( temp_output_536_0 * _Line1Alpha );
				float4 appendResult292 = (float4(temp_output_293_0 , temp_output_61_0));
				#ifdef _LINE1FEATURE_ON
				float4 staticSwitch284 = appendResult292;
				#else
				float4 staticSwitch284 = temp_cast_0;
				#endif
				float mulTime3_g168 = _TimeParameters.x * _Line2Speed;
				float2 temp_cast_2 = ((pos353*_Line2Frequency + ( mulTime3_g168 + randomoffset385 ))).xx;
				float clampResult42_g168 = clamp( ( ( tex2D( _Line2, temp_cast_2 ).r - _Line2InvertedThickness ) * _Line2Hardness ) , 0.0 , 1.0 );
				float temp_output_535_0 = clampResult42_g168;
				float3 temp_output_294_0 = (( maincolor232 * temp_output_535_0 )).rgb;
				float temp_output_69_0 = ( temp_output_535_0 * _Line2Alpha );
				float4 appendResult295 = (float4(temp_output_294_0 , temp_output_69_0));
				#ifdef _LINE2FEATURE_ON
				float4 staticSwitch285 = appendResult295;
				#else
				float4 staticSwitch285 = staticSwitch284;
				#endif
				float4 appendResult328 = (float4(0.0 , 0.0 , 0.0 , temp_output_61_0));
				float4 appendResult296 = (float4(( temp_output_293_0 * temp_output_294_0 ) , ( temp_output_61_0 * temp_output_69_0 )));
				#ifdef _LINEBOTHFEATURE_ON
				float4 staticSwitch286 = ( appendResult328 + appendResult296 );
				#else
				float4 staticSwitch286 = staticSwitch285;
				#endif
				float4 line_color298 = staticSwitch286;
				float4 temp_cast_4 = (C_ZERO287).xxxx;
				float4 screenPos238 = input.ase_texcoord6;
				float4 ase_positionSSNorm = screenPos238 / screenPos238.w;
				ase_positionSSNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_positionSSNorm.z : ase_positionSSNorm.z * 0.5 + 0.5;
				float screenDepth238 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth238 = saturate( abs( ( screenDepth238 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection1Distance ) ) );
				float saferPower344 = abs( distanceDepth238 );
				float lerpResult467 = lerp( 1.0 , pow( saferPower344 , _SoftIntersection1Intensity ) , _SoftIntersection1Affect);
				float4 appendResult321 = (float4(0.0 , 0.0 , 0.0 , lerpResult467));
				float temp_output_242_0 = ( 1.0 - distanceDepth238 );
				float4 appendResult314 = (float4((( maincolor232 * temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )).rgb , ( temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )));
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float4 staticSwitch315 = temp_cast_4;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float4 staticSwitch315 = appendResult321;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float4 staticSwitch315 = appendResult314;
				#else
				float4 staticSwitch315 = temp_cast_4;
				#endif
				float4 intersection1245 = staticSwitch315;
				float mulTime273 = _TimeParameters.x * 100.0;
				float simplePerlin3D264 = snoise( (WorldPosition*_GrainScale + mulTime273) );
				simplePerlin3D264 = simplePerlin3D264*0.5 + 0.5;
				float lerpResult278 = lerp( _GrainValues.x , _GrainValues.y , simplePerlin3D264);
				float lerpResult269 = lerp( 0.0 , lerpResult278 , _GrainAffect);
				#ifdef _GRAINFEATURE_ON
				float staticSwitch282 = lerpResult269;
				#else
				float staticSwitch282 = 0.0;
				#endif
				float grain268 = staticSwitch282;
				float4 temp_cast_6 = (C_ZERO287).xxxx;
				float screenDepth417 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth417 = saturate( abs( ( screenDepth417 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection2Distance ) ) );
				float saferPower422 = abs( distanceDepth417 );
				float lerpResult471 = lerp( 1.0 , pow( saferPower422 , _SoftIntersection2Intensity ) , _SoftIntersection2Affect);
				float4 appendResult424 = (float4(0.0 , 0.0 , 0.0 , lerpResult471));
				float temp_output_418_0 = ( 1.0 - distanceDepth417 );
				float4 appendResult425 = (float4((( maincolor232 * temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )).rgb , ( temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )));
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float4 staticSwitch429 = temp_cast_6;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float4 staticSwitch429 = appendResult424;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float4 staticSwitch429 = appendResult425;
				#else
				float4 staticSwitch429 = temp_cast_6;
				#endif
				float4 intersection2430 = staticSwitch429;
				float fresnelalpha434 = clampResult57;
				float intersectionalpha1261 = (staticSwitch315).w;
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch330 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch330 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch330 = intersectionalpha1261;
				#else
				float staticSwitch330 = intersectionalpha1261;
				#endif
				float intersectionalpha2431 = (staticSwitch429).w;
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch447 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch447 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch447 = intersectionalpha2431;
				#else
				float staticSwitch447 = intersectionalpha2431;
				#endif
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch334 = C_ONE288;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch334 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch334 = C_ONE288;
				#else
				float staticSwitch334 = C_ONE288;
				#endif
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch448 = C_ONE288;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch448 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch448 = C_ONE288;
				#else
				float staticSwitch448 = C_ONE288;
				#endif
				float clampResult62 = clamp( ( ( (maincolor232).a + fresnelalpha434 + (line_color298).w + staticSwitch330 + staticSwitch447 ) * staticSwitch334 * staticSwitch448 ) , 0.0 , 1.0 );
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.ase_texcoord7.xyz , 0.0 ) ).xyz;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float3 staticSwitch375 = WorldPosition;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float3 staticSwitch375 = input.ase_texcoord7.xyz;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float3 staticSwitch375 = temp_output_377_0;
				#else
				float3 staticSwitch375 = WorldPosition;
				#endif
				float3 vPos357 = staticSwitch375;
				float simplePerlin3D212 = snoise( ( vPos357 * _DissolveScale ) );
				simplePerlin3D212 = simplePerlin3D212*0.5 + 0.5;
				float clampResult218 = clamp( ( simplePerlin3D212 - _DissolveHide ) , 0.0 , 1.0 );
				#ifdef _DISSOLVEFEATURE_ON
				float staticSwitch337 = clampResult218;
				#else
				float staticSwitch337 = C_ONE288;
				#endif
				float dissolve432 = staticSwitch337;
				float3 objToWorld257 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				#ifdef _MASKLOCALFEATURE_ON
				float3 staticSwitch499 = ( _MaskCenter + objToWorld257 );
				#else
				float3 staticSwitch499 = _MaskCenter;
				#endif
				float clampResult256 = clamp( ( distance( max( ( abs( ( WorldPosition - staticSwitch499 ) ) - ( _MaskSize * float3( 0.5,0.5,0.5 ) ) ) , float3( 0,0,0 ) ) , float3( 0,0,0 ) ) / _MaskFalloff ) , 0.0 , 1.0 );
				float lerpResult501 = lerp( clampResult256 , ( 1.0 - clampResult256 ) , _MaskInversion);
				#ifdef _MASKFEATURE_ON
				float staticSwitch339 = lerpResult501;
				#else
				float staticSwitch339 = C_ONE288;
				#endif
				float mask254 = staticSwitch339;
				float4 _AlphaMask_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_AlphaMask_ST);
				float2 uv_AlphaMask = input.ase_texcoord5.xy * _AlphaMask_ST_Instance.xy + _AlphaMask_ST_Instance.zw;
				float lerpResult400 = lerp( C_ONE288 , tex2D( _AlphaMask, uv_AlphaMask ).r , _AlphaMaskAffect);
				#ifdef _ALPHAMASKFEATURE_ON
				float staticSwitch404 = lerpResult400;
				#else
				float staticSwitch404 = C_ONE288;
				#endif
				float alphamask402 = staticSwitch404;
				float4 appendResult325 = (float4((( staticSwitch279 * ( maincolor232 + fresnelcolor436 + float4( (line_color298).xyz , 0.0 ) + intersection1245 + grain268 + intersection2430 ) )).rgb , ( clampResult62 * dissolve432 * mask254 * alphamask402 * _Alpha )));
				float4 temp_output_504_0 = ( appendResult325 * input.ase_color );
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = temp_output_504_0.xyz;
				float Alpha = (temp_output_504_0).w;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef ASE_DEPTH_WRITE_ON
					float DepthValue = input.positionCS.z;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData = (InputData)0;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;

				#ifdef ASE_FOG
					inputData.fogCoord = InitializeInputDataFog(float4(inputData.positionWS, 1.0), input.fogFactorAndVertexLight.x);
				#endif
				#ifdef _ADDITIONAL_LIGHTS_VERTEX
					inputData.vertexLighting = input.fogFactorAndVertexLight.yzw;
				#endif

				inputData.normalizedScreenSpaceUV = NormalizedScreenSpaceUV;

				#if defined(_DBUFFER)
					ApplyDecalToBaseColor(input.positionCS, Color);
				#endif

				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						Color.rgb = MixFogColor(Color.rgb, half3(0,0,0), inputData.fogCoord);
					#else
						Color.rgb = MixFog(Color.rgb, inputData.fogCoord);
					#endif
				#endif

				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif

				#ifdef _WRITE_RENDERING_LAYERS
					uint renderingLayers = GetMeshRenderingLayer();
					outRenderingLayers = float4( EncodeMeshRenderingLayer( renderingLayers ), 0, 0, 0 );
				#endif

				return half4( Color, Alpha );
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_VERSION 19800
			#define ASE_SRP_VERSION 170003
			#define REQUIRE_DEPTH_TEXTURE 1


			#pragma vertex vert
			#pragma fragment frag

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_POSITION
			#pragma shader_feature _VOXELIZATIONFEATURE_ON
			#pragma shader_feature _LINEGLITCHFEATURE_ON
			#pragma shader_feature _POSITIONSPACEFEATURE_WORLD _POSITIONSPACEFEATURE_LOCAL _POSITIONSPACEFEATURE_CUSTOM
			#pragma shader_feature _POSITIONFEATURE_X _POSITIONFEATURE_Y _POSITIONFEATURE_Z
			#pragma multi_compile_instancing
			#pragma shader_feature _RANDOMGLITCHFEATURE_ON
			#pragma shader_feature _COLORGLITCHFEATURE_ON
			#pragma shader_feature _FRESNELFEATURE_ON
			#pragma shader_feature _NORMALMAPFEATURE_ON
			#pragma shader_feature _LINEBOTHFEATURE_ON
			#pragma shader_feature _LINE2FEATURE_ON
			#pragma shader_feature _LINE1FEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION1FEATURE_OFF _SOFTINTERSECTION1FEATURE_ALPHA _SOFTINTERSECTION1FEATURE_COLOR
			#pragma shader_feature _GRAINFEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION2FEATURE_OFF _SOFTINTERSECTION2FEATURE_ALPHA _SOFTINTERSECTION2FEATURE_COLOR
			#pragma shader_feature _DISSOLVEFEATURE_ON
			#pragma shader_feature _MASKFEATURE_ON
			#pragma shader_feature _MASKLOCALFEATURE_ON
			#pragma shader_feature _ALPHAMASKFEATURE_ON


			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE) && (SHADER_TARGET >= 45)
				#define ASE_SV_DEPTH SV_DepthLessEqual
				#define ASE_SV_POSITION_QUALIFIERS linear noperspective centroid
			#else
				#define ASE_SV_DEPTH SV_Depth
				#define ASE_SV_POSITION_QUALIFIERS
			#endif

			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				ASE_SV_POSITION_QUALIFIERS float4 positionCS : SV_POSITION;
				float4 clipPosV : TEXCOORD0;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 positionWS : TEXCOORD1;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					float4 shadowCoord : TEXCOORD2;
				#endif
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_texcoord8 : TEXCOORD8;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainColor;
			float3 _LineGlitchOffset;
			float3 _MaskSize;
			float3 _MaskCenter;
			float3 _DissolveScale;
			float3 _RandomGlitchOffset;
			float3 _GrainScale;
			float2 _GrainValues;
			float _CullMode;
			float _Line2Frequency;
			float _Line2Speed;
			float _Line2InvertedThickness;
			float _Line2Hardness;
			float _Line2Alpha;
			float _SoftIntersection1Distance;
			float _SoftIntersection1Intensity;
			float _GrainAffect;
			float _Line1Alpha;
			float _SoftIntersection2Distance;
			float _SoftIntersection2Intensity;
			float _SoftIntersection2Affect;
			float _DissolveHide;
			float _MaskFalloff;
			float _MaskInversion;
			float _SoftIntersection1Affect;
			float _Line1Hardness;
			float _Line1Speed;
			float _AlphaMaskAffect;
			float _ZWrite;
			float _PositionDirection;
			float _LineGlitchFrequency;
			float _LineGlitchSpeed;
			float _RandomOffset;
			float _LineGlitchInvertedThickness;
			float _LineGlitchHardness;
			float _RandomGlitchTiling;
			float _RandomGlitchConstant;
			float _Line1InvertedThickness;
			float _RandomGlitchAmount;
			float _VoxelizationAffect;
			float _ColorGlitchAffect;
			float _FresnelScale;
			float _FresnelPower;
			float _NormalScale;
			float _NormalAffect;
			float _FresnelAlphaScale;
			float _FresnelAlphaPower;
			float _Line1Frequency;
			float _Voxelization;
			float _Alpha;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _LineGlitch;
			sampler2D _NormalMap;
			sampler2D _Line1;
			sampler2D _Line2;
			sampler2D _AlphaMask;
			UNITY_INSTANCING_BUFFER_START(KnifeHologramShaderUnlit)
				UNITY_DEFINE_INSTANCED_PROP(float4x4, _CustomMatrix)
				UNITY_DEFINE_INSTANCED_PROP(float4, _NormalMap_ST)
				UNITY_DEFINE_INSTANCED_PROP(float4, _AlphaMask_ST)
			UNITY_INSTANCING_BUFFER_END(KnifeHologramShaderUnlit)


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float3 mod3D289( float3 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 mod3D289( float4 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 permute( float4 x ) { return mod3D289( ( x * 34.0 + 1.0 ) * x ); }
			float4 taylorInvSqrt( float4 r ) { return 1.79284291400159 - r * 0.85373472095314; }
			float snoise( float3 v )
			{
				const float2 C = float2( 1.0 / 6.0, 1.0 / 3.0 );
				float3 i = floor( v + dot( v, C.yyy ) );
				float3 x0 = v - i + dot( i, C.xxx );
				float3 g = step( x0.yzx, x0.xyz );
				float3 l = 1.0 - g;
				float3 i1 = min( g.xyz, l.zxy );
				float3 i2 = max( g.xyz, l.zxy );
				float3 x1 = x0 - i1 + C.xxx;
				float3 x2 = x0 - i2 + C.yyy;
				float3 x3 = x0 - 0.5;
				i = mod3D289( i);
				float4 p = permute( permute( permute( i.z + float4( 0.0, i1.z, i2.z, 1.0 ) ) + i.y + float4( 0.0, i1.y, i2.y, 1.0 ) ) + i.x + float4( 0.0, i1.x, i2.x, 1.0 ) );
				float4 j = p - 49.0 * floor( p / 49.0 );  // mod(p,7*7)
				float4 x_ = floor( j / 7.0 );
				float4 y_ = floor( j - 7.0 * x_ );  // mod(j,N)
				float4 x = ( x_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 y = ( y_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 h = 1.0 - abs( x ) - abs( y );
				float4 b0 = float4( x.xy, y.xy );
				float4 b1 = float4( x.zw, y.zw );
				float4 s0 = floor( b0 ) * 2.0 + 1.0;
				float4 s1 = floor( b1 ) * 2.0 + 1.0;
				float4 sh = -step( h, 0.0 );
				float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy;
				float4 a1 = b1.xzyw + s1.xzyw * sh.zzww;
				float3 g0 = float3( a0.xy, h.x );
				float3 g1 = float3( a0.zw, h.y );
				float3 g2 = float3( a1.xy, h.z );
				float3 g3 = float3( a1.zw, h.w );
				float4 norm = taylorInvSqrt( float4( dot( g0, g0 ), dot( g1, g1 ), dot( g2, g2 ), dot( g3, g3 ) ) );
				g0 *= norm.x;
				g1 *= norm.y;
				g2 *= norm.z;
				g3 *= norm.w;
				float4 m = max( 0.6 - float4( dot( x0, x0 ), dot( x1, x1 ), dot( x2, x2 ), dot( x3, x3 ) ), 0.0 );
				m = m* m;
				m = m* m;
				float4 px = float4( dot( x0, g0 ), dot( x1, g1 ), dot( x2, g2 ), dot( x3, g3 ) );
				return 42.0 * dot( m, px);
			}
			

			PackedVaryings VertexFunction( Attributes input  )
			{
				PackedVaryings output = (PackedVaryings)0;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float C_ZERO287 = 0.0;
				float3 temp_cast_0 = (C_ZERO287).xxx;
				float3 viewToObjDir94 = mul( UNITY_MATRIX_T_MV, float4( _LineGlitchOffset, 0 ) ).xyz;
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float3 ase_positionWS = TransformObjectToWorld( (input.positionOS).xyz );
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch350 = ase_positionWS.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch350 = ase_positionWS.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch350 = ase_positionWS.z;
				#else
				float staticSwitch350 = ase_positionWS.y;
				#endif
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch351 = input.positionOS.xyz.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch351 = input.positionOS.xyz.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch351 = input.positionOS.xyz.z;
				#else
				float staticSwitch351 = input.positionOS.xyz.y;
				#endif
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.positionOS.xyz , 0.0 ) ).xyz;
				float3 break379 = temp_output_377_0;
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch378 = break379.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch378 = break379.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch378 = break379.z;
				#else
				float staticSwitch378 = break379.y;
				#endif
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch352 = staticSwitch350;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch352 = staticSwitch351;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch352 = staticSwitch378;
				#else
				float staticSwitch352 = staticSwitch350;
				#endif
				float vertexToFrag497 = ( staticSwitch352 * _PositionDirection );
				float pos353 = vertexToFrag497;
				float mulTime3_g167 = _TimeParameters.x * _LineGlitchSpeed;
				float randomoffset385 = _RandomOffset;
				float2 temp_cast_3 = ((pos353*_LineGlitchFrequency + ( mulTime3_g167 + randomoffset385 ))).xx;
				float clampResult42_g167 = clamp( ( ( tex2Dlod( _LineGlitch, float4( temp_cast_3, 0, 0.0) ).r - _LineGlitchInvertedThickness ) * _LineGlitchHardness ) , 0.0 , 1.0 );
				#ifdef _LINEGLITCHFEATURE_ON
				float3 staticSwitch307 = ( ( viewToObjDir94 / ase_objectScale ) * clampResult42_g167 );
				#else
				float3 staticSwitch307 = temp_cast_0;
				#endif
				float3 lineglitch491 = staticSwitch307;
				float3 temp_cast_4 = (C_ZERO287).xxx;
				float3 viewToObjDir122 = mul( UNITY_MATRIX_T_MV, float4( _RandomGlitchOffset, 0 ) ).xyz;
				float mulTime149 = _TimeParameters.x * -2.3;
				float mulTime155 = _TimeParameters.x * -2.05;
				float2 appendResult153 = (float2((pos353*_RandomGlitchTiling + ( mulTime149 + randomoffset385 )) , ( randomoffset385 + mulTime155 )));
				float simplePerlin2D186 = snoise( appendResult153 );
				simplePerlin2D186 = simplePerlin2D186*0.5 + 0.5;
				float4 matrixToPos373 = float4( float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[0][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[1][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[2][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[3][3]);
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch365 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch365 = 0.0;
				#else
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#endif
				float mulTime139 = _TimeParameters.x * -5.74;
				float mulTime157 = _TimeParameters.x * -0.83;
				float2 appendResult158 = (float2((staticSwitch365*223.0 + ( mulTime139 + randomoffset385 )) , ( randomoffset385 + mulTime157 )));
				float simplePerlin2D187 = snoise( appendResult158 );
				simplePerlin2D187 = simplePerlin2D187*0.5 + 0.5;
				float clampResult148 = clamp( (-1.0 + (( simplePerlin2D187 + _RandomGlitchConstant ) - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float temp_output_197_0 = ( (-1.0 + (simplePerlin2D186 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) * clampResult148 );
				float2 break190 = appendResult153;
				float2 appendResult195 = (float2(( 20.0 * break190.x ) , break190.y));
				float simplePerlin2D188 = snoise( appendResult195 );
				simplePerlin2D188 = simplePerlin2D188*0.5 + 0.5;
				float clampResult192 = clamp( (-1.0 + (simplePerlin2D188 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult199 = lerp( 0.0 , clampResult192 , 2.0);
				#ifdef _RANDOMGLITCHFEATURE_ON
				float3 staticSwitch311 = ( ( viewToObjDir122 / ase_objectScale ) * ( temp_output_197_0 + ( temp_output_197_0 * lerpResult199 ) ) * _RandomGlitchAmount );
				#else
				float3 staticSwitch311 = temp_cast_4;
				#endif
				float3 randomglitch493 = staticSwitch311;
				float3 vertexoffset410 = ( lineglitch491 + randomglitch493 );
				float3 temp_output_512_0 = ( input.positionOS.xyz + vertexoffset410 );
				float3 lerpResult517 = lerp( temp_output_512_0 , ( round( ( temp_output_512_0 * _Voxelization ) ) / _Voxelization ) , _VoxelizationAffect);
				#ifdef _VOXELIZATIONFEATURE_ON
				float3 staticSwitch518 = lerpResult517;
				#else
				float3 staticSwitch518 = temp_output_512_0;
				#endif
				float3 ModifiedVertexPosition519 = staticSwitch518;
				
				float3 ase_normalWS = TransformObjectToWorldNormal(input.normalOS);
				output.ase_texcoord3.xyz = ase_normalWS;
				float3 ase_tangentWS = TransformObjectToWorldDir(input.ase_tangent.xyz);
				output.ase_texcoord5.xyz = ase_tangentWS;
				float ase_tangentSign = input.ase_tangent.w * ( unity_WorldTransformParams.w >= 0.0 ? 1.0 : -1.0 );
				float3 ase_bitangentWS = cross( ase_normalWS, ase_tangentWS ) * ase_tangentSign;
				output.ase_texcoord6.xyz = ase_bitangentWS;
				output.ase_texcoord3.w = vertexToFrag497;
				float3 vertexPos238 = ( input.positionOS.xyz + vertexoffset410 );
				float4 ase_positionCS238 = TransformObjectToHClip((vertexPos238).xyz);
				float4 screenPos238 = ComputeScreenPos(ase_positionCS238);
				output.ase_texcoord7 = screenPos238;
				float3 vertexPos417 = ( input.positionOS.xyz + vertexoffset410 );
				
				output.ase_texcoord4.xy = input.ase_texcoord.xy;
				output.ase_texcoord8 = input.positionOS;
				output.ase_color = input.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord4.zw = 0;
				output.ase_texcoord5.w = 0;
				output.ase_texcoord6.w = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ModifiedVertexPosition519;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				VertexPositionInputs vertexInput = GetVertexPositionInputs( input.positionOS.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					output.positionWS = vertexInput.positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					output.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				output.positionCS = vertexInput.positionCS;
				output.clipPosV = vertexInput.positionCS;
				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 positionOS : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.positionOS = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				output.ase_tangent = input.ase_tangent;
				output.ase_color = input.ase_color;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].positionOS, input[1].positionOS, input[2].positionOS, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].positionOS, input[1].positionOS, input[2].positionOS, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].positionOS, input[1].positionOS, input[2].positionOS, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].positionOS * bary.x + patch[1].positionOS * bary.y + patch[2].positionOS * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				output.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				output.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].positionOS.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			half4 frag(PackedVaryings input
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( input );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = input.positionWS;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				float4 ClipPos = input.clipPosV;
				float4 ScreenPos = ComputeScreenPos( input.clipPosV );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = input.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float3 objToWorld161 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				float C_ZERO287 = 0.0;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch360 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch360 = 0.0;
				#else
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#endif
				float randomoffset385 = _RandomOffset;
				float mulTime165 = _TimeParameters.x * -15.0;
				float mulTime167 = _TimeParameters.x * -0.5;
				float2 appendResult168 = (float2((staticSwitch360*223.0 + ( randomoffset385 + mulTime165 )) , ( randomoffset385 + mulTime167 )));
				float simplePerlin2D176 = snoise( appendResult168 );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float clampResult175 = clamp( (-0.61 + (simplePerlin2D176 - 0.0) * (2.0 - -0.61) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult181 = lerp( 1.0 , clampResult175 , _ColorGlitchAffect);
				#ifdef _COLORGLITCHFEATURE_ON
				float staticSwitch279 = lerpResult181;
				#else
				float staticSwitch279 = 1.0;
				#endif
				float4 maincolor232 = _MainColor;
				float C_ONE288 = 1.0;
				float3 ase_viewVectorWS = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				float3 ase_viewDirWS = normalize( ase_viewVectorWS );
				float3 ase_normalWS = input.ase_texcoord3.xyz;
				float fresnelNdotV2 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode2 = ( 0.0 + _FresnelScale * pow( max( 1.0 - fresnelNdotV2 , 0.0001 ), _FresnelPower ) );
				float4 _NormalMap_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_NormalMap_ST);
				float2 uv_NormalMap = input.ase_texcoord4.xy * _NormalMap_ST_Instance.xy + _NormalMap_ST_Instance.zw;
				float3 unpack95 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalScale );
				unpack95.z = lerp( 1, unpack95.z, saturate(_NormalScale) );
				float3 ase_tangentWS = input.ase_texcoord5.xyz;
				float3 ase_bitangentWS = input.ase_texcoord6.xyz;
				float3 tanToWorld0 = float3( ase_tangentWS.x, ase_bitangentWS.x, ase_normalWS.x );
				float3 tanToWorld1 = float3( ase_tangentWS.y, ase_bitangentWS.y, ase_normalWS.y );
				float3 tanToWorld2 = float3( ase_tangentWS.z, ase_bitangentWS.z, ase_normalWS.z );
				float3 ase_viewVectorTS =  tanToWorld0 * ( _WorldSpaceCameraPos.xyz - WorldPosition ).x + tanToWorld1 * ( _WorldSpaceCameraPos.xyz - WorldPosition ).y  + tanToWorld2 * ( _WorldSpaceCameraPos.xyz - WorldPosition ).z;
				float3 ase_viewDirTS = normalize( ase_viewVectorTS );
				float dotResult101 = dot( unpack95 , ase_viewDirTS );
				float lerpResult106 = lerp( 1.0 , (0.0 + (dotResult101 - -1.0) * (1.0 - 0.0) / (1.0 - -1.0)) , _NormalAffect);
				#ifdef _NORMALMAPFEATURE_ON
				float staticSwitch341 = ( 1.0 - lerpResult106 );
				#else
				float staticSwitch341 = C_ZERO287;
				#endif
				float NormalAffect116 = staticSwitch341;
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch303 = ( fresnelNode2 + NormalAffect116 );
				#else
				float staticSwitch303 = C_ONE288;
				#endif
				float fresnelNdotV53 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode53 = ( 0.0 + _FresnelAlphaScale * pow( max( 1.0 - fresnelNdotV53 , 0.0001 ), _FresnelAlphaPower ) );
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch305 = fresnelNode53;
				#else
				float staticSwitch305 = C_ZERO287;
				#endif
				float clampResult57 = clamp( ( staticSwitch305 + NormalAffect116 ) , 0.0 , 1.0 );
				float4 fresnelcolor436 = ( staticSwitch303 * maincolor232 * clampResult57 );
				float4 temp_cast_0 = (C_ZERO287).xxxx;
				float vertexToFrag497 = input.ase_texcoord3.w;
				float pos353 = vertexToFrag497;
				float mulTime3_g169 = _TimeParameters.x * _Line1Speed;
				float2 temp_cast_1 = ((pos353*_Line1Frequency + ( mulTime3_g169 + randomoffset385 ))).xx;
				float clampResult42_g169 = clamp( ( ( tex2D( _Line1, temp_cast_1 ).r - _Line1InvertedThickness ) * _Line1Hardness ) , 0.0 , 1.0 );
				float temp_output_536_0 = clampResult42_g169;
				float3 temp_output_293_0 = (( maincolor232 * temp_output_536_0 )).rgb;
				float temp_output_61_0 = ( temp_output_536_0 * _Line1Alpha );
				float4 appendResult292 = (float4(temp_output_293_0 , temp_output_61_0));
				#ifdef _LINE1FEATURE_ON
				float4 staticSwitch284 = appendResult292;
				#else
				float4 staticSwitch284 = temp_cast_0;
				#endif
				float mulTime3_g168 = _TimeParameters.x * _Line2Speed;
				float2 temp_cast_2 = ((pos353*_Line2Frequency + ( mulTime3_g168 + randomoffset385 ))).xx;
				float clampResult42_g168 = clamp( ( ( tex2D( _Line2, temp_cast_2 ).r - _Line2InvertedThickness ) * _Line2Hardness ) , 0.0 , 1.0 );
				float temp_output_535_0 = clampResult42_g168;
				float3 temp_output_294_0 = (( maincolor232 * temp_output_535_0 )).rgb;
				float temp_output_69_0 = ( temp_output_535_0 * _Line2Alpha );
				float4 appendResult295 = (float4(temp_output_294_0 , temp_output_69_0));
				#ifdef _LINE2FEATURE_ON
				float4 staticSwitch285 = appendResult295;
				#else
				float4 staticSwitch285 = staticSwitch284;
				#endif
				float4 appendResult328 = (float4(0.0 , 0.0 , 0.0 , temp_output_61_0));
				float4 appendResult296 = (float4(( temp_output_293_0 * temp_output_294_0 ) , ( temp_output_61_0 * temp_output_69_0 )));
				#ifdef _LINEBOTHFEATURE_ON
				float4 staticSwitch286 = ( appendResult328 + appendResult296 );
				#else
				float4 staticSwitch286 = staticSwitch285;
				#endif
				float4 line_color298 = staticSwitch286;
				float4 temp_cast_4 = (C_ZERO287).xxxx;
				float4 screenPos238 = input.ase_texcoord7;
				float4 ase_positionSSNorm = screenPos238 / screenPos238.w;
				ase_positionSSNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_positionSSNorm.z : ase_positionSSNorm.z * 0.5 + 0.5;
				float screenDepth238 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth238 = saturate( abs( ( screenDepth238 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection1Distance ) ) );
				float saferPower344 = abs( distanceDepth238 );
				float lerpResult467 = lerp( 1.0 , pow( saferPower344 , _SoftIntersection1Intensity ) , _SoftIntersection1Affect);
				float4 appendResult321 = (float4(0.0 , 0.0 , 0.0 , lerpResult467));
				float temp_output_242_0 = ( 1.0 - distanceDepth238 );
				float4 appendResult314 = (float4((( maincolor232 * temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )).rgb , ( temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )));
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float4 staticSwitch315 = temp_cast_4;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float4 staticSwitch315 = appendResult321;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float4 staticSwitch315 = appendResult314;
				#else
				float4 staticSwitch315 = temp_cast_4;
				#endif
				float4 intersection1245 = staticSwitch315;
				float mulTime273 = _TimeParameters.x * 100.0;
				float simplePerlin3D264 = snoise( (WorldPosition*_GrainScale + mulTime273) );
				simplePerlin3D264 = simplePerlin3D264*0.5 + 0.5;
				float lerpResult278 = lerp( _GrainValues.x , _GrainValues.y , simplePerlin3D264);
				float lerpResult269 = lerp( 0.0 , lerpResult278 , _GrainAffect);
				#ifdef _GRAINFEATURE_ON
				float staticSwitch282 = lerpResult269;
				#else
				float staticSwitch282 = 0.0;
				#endif
				float grain268 = staticSwitch282;
				float4 temp_cast_6 = (C_ZERO287).xxxx;
				float screenDepth417 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth417 = saturate( abs( ( screenDepth417 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection2Distance ) ) );
				float saferPower422 = abs( distanceDepth417 );
				float lerpResult471 = lerp( 1.0 , pow( saferPower422 , _SoftIntersection2Intensity ) , _SoftIntersection2Affect);
				float4 appendResult424 = (float4(0.0 , 0.0 , 0.0 , lerpResult471));
				float temp_output_418_0 = ( 1.0 - distanceDepth417 );
				float4 appendResult425 = (float4((( maincolor232 * temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )).rgb , ( temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )));
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float4 staticSwitch429 = temp_cast_6;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float4 staticSwitch429 = appendResult424;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float4 staticSwitch429 = appendResult425;
				#else
				float4 staticSwitch429 = temp_cast_6;
				#endif
				float4 intersection2430 = staticSwitch429;
				float fresnelalpha434 = clampResult57;
				float intersectionalpha1261 = (staticSwitch315).w;
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch330 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch330 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch330 = intersectionalpha1261;
				#else
				float staticSwitch330 = intersectionalpha1261;
				#endif
				float intersectionalpha2431 = (staticSwitch429).w;
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch447 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch447 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch447 = intersectionalpha2431;
				#else
				float staticSwitch447 = intersectionalpha2431;
				#endif
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch334 = C_ONE288;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch334 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch334 = C_ONE288;
				#else
				float staticSwitch334 = C_ONE288;
				#endif
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch448 = C_ONE288;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch448 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch448 = C_ONE288;
				#else
				float staticSwitch448 = C_ONE288;
				#endif
				float clampResult62 = clamp( ( ( (maincolor232).a + fresnelalpha434 + (line_color298).w + staticSwitch330 + staticSwitch447 ) * staticSwitch334 * staticSwitch448 ) , 0.0 , 1.0 );
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.ase_texcoord8.xyz , 0.0 ) ).xyz;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float3 staticSwitch375 = WorldPosition;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float3 staticSwitch375 = input.ase_texcoord8.xyz;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float3 staticSwitch375 = temp_output_377_0;
				#else
				float3 staticSwitch375 = WorldPosition;
				#endif
				float3 vPos357 = staticSwitch375;
				float simplePerlin3D212 = snoise( ( vPos357 * _DissolveScale ) );
				simplePerlin3D212 = simplePerlin3D212*0.5 + 0.5;
				float clampResult218 = clamp( ( simplePerlin3D212 - _DissolveHide ) , 0.0 , 1.0 );
				#ifdef _DISSOLVEFEATURE_ON
				float staticSwitch337 = clampResult218;
				#else
				float staticSwitch337 = C_ONE288;
				#endif
				float dissolve432 = staticSwitch337;
				float3 objToWorld257 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				#ifdef _MASKLOCALFEATURE_ON
				float3 staticSwitch499 = ( _MaskCenter + objToWorld257 );
				#else
				float3 staticSwitch499 = _MaskCenter;
				#endif
				float clampResult256 = clamp( ( distance( max( ( abs( ( WorldPosition - staticSwitch499 ) ) - ( _MaskSize * float3( 0.5,0.5,0.5 ) ) ) , float3( 0,0,0 ) ) , float3( 0,0,0 ) ) / _MaskFalloff ) , 0.0 , 1.0 );
				float lerpResult501 = lerp( clampResult256 , ( 1.0 - clampResult256 ) , _MaskInversion);
				#ifdef _MASKFEATURE_ON
				float staticSwitch339 = lerpResult501;
				#else
				float staticSwitch339 = C_ONE288;
				#endif
				float mask254 = staticSwitch339;
				float4 _AlphaMask_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_AlphaMask_ST);
				float2 uv_AlphaMask = input.ase_texcoord4.xy * _AlphaMask_ST_Instance.xy + _AlphaMask_ST_Instance.zw;
				float lerpResult400 = lerp( C_ONE288 , tex2D( _AlphaMask, uv_AlphaMask ).r , _AlphaMaskAffect);
				#ifdef _ALPHAMASKFEATURE_ON
				float staticSwitch404 = lerpResult400;
				#else
				float staticSwitch404 = C_ONE288;
				#endif
				float alphamask402 = staticSwitch404;
				float4 appendResult325 = (float4((( staticSwitch279 * ( maincolor232 + fresnelcolor436 + float4( (line_color298).xyz , 0.0 ) + intersection1245 + grain268 + intersection2430 ) )).rgb , ( clampResult62 * dissolve432 * mask254 * alphamask402 * _Alpha )));
				float4 temp_output_504_0 = ( appendResult325 * input.ase_color );
				

				float Alpha = (temp_output_504_0).w;
				float AlphaClipThreshold = 0.5;

				#ifdef ASE_DEPTH_WRITE_ON
					float DepthValue = input.positionCS.z;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#if defined(LOD_FADE_CROSSFADE)
					LODFadeCrossFade( input.positionCS );
				#endif

				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif

				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "SceneSelectionPass"
			Tags { "LightMode"="SceneSelectionPass" }

			Cull Off
			AlphaToMask Off

			HLSLPROGRAM

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_VERSION 19800
			#define ASE_SRP_VERSION 170003
			#define REQUIRE_DEPTH_TEXTURE 1


			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRenderingKeywords.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRendering.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_POSITION
			#pragma shader_feature _VOXELIZATIONFEATURE_ON
			#pragma shader_feature _LINEGLITCHFEATURE_ON
			#pragma shader_feature _POSITIONSPACEFEATURE_WORLD _POSITIONSPACEFEATURE_LOCAL _POSITIONSPACEFEATURE_CUSTOM
			#pragma shader_feature _POSITIONFEATURE_X _POSITIONFEATURE_Y _POSITIONFEATURE_Z
			#pragma multi_compile_instancing
			#pragma shader_feature _RANDOMGLITCHFEATURE_ON
			#pragma shader_feature _COLORGLITCHFEATURE_ON
			#pragma shader_feature _FRESNELFEATURE_ON
			#pragma shader_feature _NORMALMAPFEATURE_ON
			#pragma shader_feature _LINEBOTHFEATURE_ON
			#pragma shader_feature _LINE2FEATURE_ON
			#pragma shader_feature _LINE1FEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION1FEATURE_OFF _SOFTINTERSECTION1FEATURE_ALPHA _SOFTINTERSECTION1FEATURE_COLOR
			#pragma shader_feature _GRAINFEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION2FEATURE_OFF _SOFTINTERSECTION2FEATURE_ALPHA _SOFTINTERSECTION2FEATURE_COLOR
			#pragma shader_feature _DISSOLVEFEATURE_ON
			#pragma shader_feature _MASKFEATURE_ON
			#pragma shader_feature _MASKLOCALFEATURE_ON
			#pragma shader_feature _ALPHAMASKFEATURE_ON


			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				float4 positionCS : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainColor;
			float3 _LineGlitchOffset;
			float3 _MaskSize;
			float3 _MaskCenter;
			float3 _DissolveScale;
			float3 _RandomGlitchOffset;
			float3 _GrainScale;
			float2 _GrainValues;
			float _CullMode;
			float _Line2Frequency;
			float _Line2Speed;
			float _Line2InvertedThickness;
			float _Line2Hardness;
			float _Line2Alpha;
			float _SoftIntersection1Distance;
			float _SoftIntersection1Intensity;
			float _GrainAffect;
			float _Line1Alpha;
			float _SoftIntersection2Distance;
			float _SoftIntersection2Intensity;
			float _SoftIntersection2Affect;
			float _DissolveHide;
			float _MaskFalloff;
			float _MaskInversion;
			float _SoftIntersection1Affect;
			float _Line1Hardness;
			float _Line1Speed;
			float _AlphaMaskAffect;
			float _ZWrite;
			float _PositionDirection;
			float _LineGlitchFrequency;
			float _LineGlitchSpeed;
			float _RandomOffset;
			float _LineGlitchInvertedThickness;
			float _LineGlitchHardness;
			float _RandomGlitchTiling;
			float _RandomGlitchConstant;
			float _Line1InvertedThickness;
			float _RandomGlitchAmount;
			float _VoxelizationAffect;
			float _ColorGlitchAffect;
			float _FresnelScale;
			float _FresnelPower;
			float _NormalScale;
			float _NormalAffect;
			float _FresnelAlphaScale;
			float _FresnelAlphaPower;
			float _Line1Frequency;
			float _Voxelization;
			float _Alpha;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _LineGlitch;
			sampler2D _NormalMap;
			sampler2D _Line1;
			sampler2D _Line2;
			sampler2D _AlphaMask;
			UNITY_INSTANCING_BUFFER_START(KnifeHologramShaderUnlit)
				UNITY_DEFINE_INSTANCED_PROP(float4x4, _CustomMatrix)
				UNITY_DEFINE_INSTANCED_PROP(float4, _NormalMap_ST)
				UNITY_DEFINE_INSTANCED_PROP(float4, _AlphaMask_ST)
			UNITY_INSTANCING_BUFFER_END(KnifeHologramShaderUnlit)


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float3 mod3D289( float3 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 mod3D289( float4 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 permute( float4 x ) { return mod3D289( ( x * 34.0 + 1.0 ) * x ); }
			float4 taylorInvSqrt( float4 r ) { return 1.79284291400159 - r * 0.85373472095314; }
			float snoise( float3 v )
			{
				const float2 C = float2( 1.0 / 6.0, 1.0 / 3.0 );
				float3 i = floor( v + dot( v, C.yyy ) );
				float3 x0 = v - i + dot( i, C.xxx );
				float3 g = step( x0.yzx, x0.xyz );
				float3 l = 1.0 - g;
				float3 i1 = min( g.xyz, l.zxy );
				float3 i2 = max( g.xyz, l.zxy );
				float3 x1 = x0 - i1 + C.xxx;
				float3 x2 = x0 - i2 + C.yyy;
				float3 x3 = x0 - 0.5;
				i = mod3D289( i);
				float4 p = permute( permute( permute( i.z + float4( 0.0, i1.z, i2.z, 1.0 ) ) + i.y + float4( 0.0, i1.y, i2.y, 1.0 ) ) + i.x + float4( 0.0, i1.x, i2.x, 1.0 ) );
				float4 j = p - 49.0 * floor( p / 49.0 );  // mod(p,7*7)
				float4 x_ = floor( j / 7.0 );
				float4 y_ = floor( j - 7.0 * x_ );  // mod(j,N)
				float4 x = ( x_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 y = ( y_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 h = 1.0 - abs( x ) - abs( y );
				float4 b0 = float4( x.xy, y.xy );
				float4 b1 = float4( x.zw, y.zw );
				float4 s0 = floor( b0 ) * 2.0 + 1.0;
				float4 s1 = floor( b1 ) * 2.0 + 1.0;
				float4 sh = -step( h, 0.0 );
				float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy;
				float4 a1 = b1.xzyw + s1.xzyw * sh.zzww;
				float3 g0 = float3( a0.xy, h.x );
				float3 g1 = float3( a0.zw, h.y );
				float3 g2 = float3( a1.xy, h.z );
				float3 g3 = float3( a1.zw, h.w );
				float4 norm = taylorInvSqrt( float4( dot( g0, g0 ), dot( g1, g1 ), dot( g2, g2 ), dot( g3, g3 ) ) );
				g0 *= norm.x;
				g1 *= norm.y;
				g2 *= norm.z;
				g3 *= norm.w;
				float4 m = max( 0.6 - float4( dot( x0, x0 ), dot( x1, x1 ), dot( x2, x2 ), dot( x3, x3 ) ), 0.0 );
				m = m* m;
				m = m* m;
				float4 px = float4( dot( x0, g0 ), dot( x1, g1 ), dot( x2, g2 ), dot( x3, g3 ) );
				return 42.0 * dot( m, px);
			}
			

			int _ObjectId;
			int _PassValue;

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			PackedVaryings VertexFunction(Attributes input  )
			{
				PackedVaryings output;
				ZERO_INITIALIZE(PackedVaryings, output);

				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float C_ZERO287 = 0.0;
				float3 temp_cast_0 = (C_ZERO287).xxx;
				float3 viewToObjDir94 = mul( UNITY_MATRIX_T_MV, float4( _LineGlitchOffset, 0 ) ).xyz;
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float3 ase_positionWS = TransformObjectToWorld( (input.positionOS).xyz );
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch350 = ase_positionWS.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch350 = ase_positionWS.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch350 = ase_positionWS.z;
				#else
				float staticSwitch350 = ase_positionWS.y;
				#endif
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch351 = input.positionOS.xyz.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch351 = input.positionOS.xyz.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch351 = input.positionOS.xyz.z;
				#else
				float staticSwitch351 = input.positionOS.xyz.y;
				#endif
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.positionOS.xyz , 0.0 ) ).xyz;
				float3 break379 = temp_output_377_0;
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch378 = break379.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch378 = break379.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch378 = break379.z;
				#else
				float staticSwitch378 = break379.y;
				#endif
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch352 = staticSwitch350;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch352 = staticSwitch351;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch352 = staticSwitch378;
				#else
				float staticSwitch352 = staticSwitch350;
				#endif
				float vertexToFrag497 = ( staticSwitch352 * _PositionDirection );
				float pos353 = vertexToFrag497;
				float mulTime3_g167 = _TimeParameters.x * _LineGlitchSpeed;
				float randomoffset385 = _RandomOffset;
				float2 temp_cast_3 = ((pos353*_LineGlitchFrequency + ( mulTime3_g167 + randomoffset385 ))).xx;
				float clampResult42_g167 = clamp( ( ( tex2Dlod( _LineGlitch, float4( temp_cast_3, 0, 0.0) ).r - _LineGlitchInvertedThickness ) * _LineGlitchHardness ) , 0.0 , 1.0 );
				#ifdef _LINEGLITCHFEATURE_ON
				float3 staticSwitch307 = ( ( viewToObjDir94 / ase_objectScale ) * clampResult42_g167 );
				#else
				float3 staticSwitch307 = temp_cast_0;
				#endif
				float3 lineglitch491 = staticSwitch307;
				float3 temp_cast_4 = (C_ZERO287).xxx;
				float3 viewToObjDir122 = mul( UNITY_MATRIX_T_MV, float4( _RandomGlitchOffset, 0 ) ).xyz;
				float mulTime149 = _TimeParameters.x * -2.3;
				float mulTime155 = _TimeParameters.x * -2.05;
				float2 appendResult153 = (float2((pos353*_RandomGlitchTiling + ( mulTime149 + randomoffset385 )) , ( randomoffset385 + mulTime155 )));
				float simplePerlin2D186 = snoise( appendResult153 );
				simplePerlin2D186 = simplePerlin2D186*0.5 + 0.5;
				float4 matrixToPos373 = float4( float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[0][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[1][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[2][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[3][3]);
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch365 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch365 = 0.0;
				#else
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#endif
				float mulTime139 = _TimeParameters.x * -5.74;
				float mulTime157 = _TimeParameters.x * -0.83;
				float2 appendResult158 = (float2((staticSwitch365*223.0 + ( mulTime139 + randomoffset385 )) , ( randomoffset385 + mulTime157 )));
				float simplePerlin2D187 = snoise( appendResult158 );
				simplePerlin2D187 = simplePerlin2D187*0.5 + 0.5;
				float clampResult148 = clamp( (-1.0 + (( simplePerlin2D187 + _RandomGlitchConstant ) - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float temp_output_197_0 = ( (-1.0 + (simplePerlin2D186 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) * clampResult148 );
				float2 break190 = appendResult153;
				float2 appendResult195 = (float2(( 20.0 * break190.x ) , break190.y));
				float simplePerlin2D188 = snoise( appendResult195 );
				simplePerlin2D188 = simplePerlin2D188*0.5 + 0.5;
				float clampResult192 = clamp( (-1.0 + (simplePerlin2D188 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult199 = lerp( 0.0 , clampResult192 , 2.0);
				#ifdef _RANDOMGLITCHFEATURE_ON
				float3 staticSwitch311 = ( ( viewToObjDir122 / ase_objectScale ) * ( temp_output_197_0 + ( temp_output_197_0 * lerpResult199 ) ) * _RandomGlitchAmount );
				#else
				float3 staticSwitch311 = temp_cast_4;
				#endif
				float3 randomglitch493 = staticSwitch311;
				float3 vertexoffset410 = ( lineglitch491 + randomglitch493 );
				float3 temp_output_512_0 = ( input.positionOS.xyz + vertexoffset410 );
				float3 lerpResult517 = lerp( temp_output_512_0 , ( round( ( temp_output_512_0 * _Voxelization ) ) / _Voxelization ) , _VoxelizationAffect);
				#ifdef _VOXELIZATIONFEATURE_ON
				float3 staticSwitch518 = lerpResult517;
				#else
				float3 staticSwitch518 = temp_output_512_0;
				#endif
				float3 ModifiedVertexPosition519 = staticSwitch518;
				
				output.ase_texcoord.xyz = ase_positionWS;
				float3 ase_normalWS = TransformObjectToWorldNormal(input.normalOS);
				output.ase_texcoord1.xyz = ase_normalWS;
				float3 ase_tangentWS = TransformObjectToWorldDir(input.ase_tangent.xyz);
				output.ase_texcoord3.xyz = ase_tangentWS;
				float ase_tangentSign = input.ase_tangent.w * ( unity_WorldTransformParams.w >= 0.0 ? 1.0 : -1.0 );
				float3 ase_bitangentWS = cross( ase_normalWS, ase_tangentWS ) * ase_tangentSign;
				output.ase_texcoord4.xyz = ase_bitangentWS;
				output.ase_texcoord.w = vertexToFrag497;
				float3 vertexPos238 = ( input.positionOS.xyz + vertexoffset410 );
				float4 ase_positionCS238 = TransformObjectToHClip((vertexPos238).xyz);
				float4 screenPos238 = ComputeScreenPos(ase_positionCS238);
				output.ase_texcoord5 = screenPos238;
				float3 vertexPos417 = ( input.positionOS.xyz + vertexoffset410 );
				
				output.ase_texcoord2.xy = input.ase_texcoord.xy;
				output.ase_texcoord6 = input.positionOS;
				output.ase_color = input.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord1.w = 0;
				output.ase_texcoord2.zw = 0;
				output.ase_texcoord3.w = 0;
				output.ase_texcoord4.w = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ModifiedVertexPosition519;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				float3 positionWS = TransformObjectToWorld( input.positionOS.xyz );

				output.positionCS = TransformWorldToHClip(positionWS);

				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 positionOS : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.positionOS = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				output.ase_tangent = input.ase_tangent;
				output.ase_color = input.ase_color;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].positionOS, input[1].positionOS, input[2].positionOS, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].positionOS, input[1].positionOS, input[2].positionOS, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].positionOS, input[1].positionOS, input[2].positionOS, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].positionOS * bary.x + patch[1].positionOS * bary.y + patch[2].positionOS * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				output.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				output.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].positionOS.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			half4 frag(PackedVaryings input ) : SV_Target
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float3 objToWorld161 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				float C_ZERO287 = 0.0;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch360 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch360 = 0.0;
				#else
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#endif
				float randomoffset385 = _RandomOffset;
				float mulTime165 = _TimeParameters.x * -15.0;
				float mulTime167 = _TimeParameters.x * -0.5;
				float2 appendResult168 = (float2((staticSwitch360*223.0 + ( randomoffset385 + mulTime165 )) , ( randomoffset385 + mulTime167 )));
				float simplePerlin2D176 = snoise( appendResult168 );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float clampResult175 = clamp( (-0.61 + (simplePerlin2D176 - 0.0) * (2.0 - -0.61) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult181 = lerp( 1.0 , clampResult175 , _ColorGlitchAffect);
				#ifdef _COLORGLITCHFEATURE_ON
				float staticSwitch279 = lerpResult181;
				#else
				float staticSwitch279 = 1.0;
				#endif
				float4 maincolor232 = _MainColor;
				float C_ONE288 = 1.0;
				float3 ase_positionWS = input.ase_texcoord.xyz;
				float3 ase_viewVectorWS = ( _WorldSpaceCameraPos.xyz - ase_positionWS );
				float3 ase_viewDirWS = normalize( ase_viewVectorWS );
				float3 ase_normalWS = input.ase_texcoord1.xyz;
				float fresnelNdotV2 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode2 = ( 0.0 + _FresnelScale * pow( max( 1.0 - fresnelNdotV2 , 0.0001 ), _FresnelPower ) );
				float4 _NormalMap_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_NormalMap_ST);
				float2 uv_NormalMap = input.ase_texcoord2.xy * _NormalMap_ST_Instance.xy + _NormalMap_ST_Instance.zw;
				float3 unpack95 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalScale );
				unpack95.z = lerp( 1, unpack95.z, saturate(_NormalScale) );
				float3 ase_tangentWS = input.ase_texcoord3.xyz;
				float3 ase_bitangentWS = input.ase_texcoord4.xyz;
				float3 tanToWorld0 = float3( ase_tangentWS.x, ase_bitangentWS.x, ase_normalWS.x );
				float3 tanToWorld1 = float3( ase_tangentWS.y, ase_bitangentWS.y, ase_normalWS.y );
				float3 tanToWorld2 = float3( ase_tangentWS.z, ase_bitangentWS.z, ase_normalWS.z );
				float3 ase_viewVectorTS =  tanToWorld0 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).x + tanToWorld1 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).y  + tanToWorld2 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).z;
				float3 ase_viewDirTS = normalize( ase_viewVectorTS );
				float dotResult101 = dot( unpack95 , ase_viewDirTS );
				float lerpResult106 = lerp( 1.0 , (0.0 + (dotResult101 - -1.0) * (1.0 - 0.0) / (1.0 - -1.0)) , _NormalAffect);
				#ifdef _NORMALMAPFEATURE_ON
				float staticSwitch341 = ( 1.0 - lerpResult106 );
				#else
				float staticSwitch341 = C_ZERO287;
				#endif
				float NormalAffect116 = staticSwitch341;
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch303 = ( fresnelNode2 + NormalAffect116 );
				#else
				float staticSwitch303 = C_ONE288;
				#endif
				float fresnelNdotV53 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode53 = ( 0.0 + _FresnelAlphaScale * pow( max( 1.0 - fresnelNdotV53 , 0.0001 ), _FresnelAlphaPower ) );
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch305 = fresnelNode53;
				#else
				float staticSwitch305 = C_ZERO287;
				#endif
				float clampResult57 = clamp( ( staticSwitch305 + NormalAffect116 ) , 0.0 , 1.0 );
				float4 fresnelcolor436 = ( staticSwitch303 * maincolor232 * clampResult57 );
				float4 temp_cast_0 = (C_ZERO287).xxxx;
				float vertexToFrag497 = input.ase_texcoord.w;
				float pos353 = vertexToFrag497;
				float mulTime3_g169 = _TimeParameters.x * _Line1Speed;
				float2 temp_cast_1 = ((pos353*_Line1Frequency + ( mulTime3_g169 + randomoffset385 ))).xx;
				float clampResult42_g169 = clamp( ( ( tex2D( _Line1, temp_cast_1 ).r - _Line1InvertedThickness ) * _Line1Hardness ) , 0.0 , 1.0 );
				float temp_output_536_0 = clampResult42_g169;
				float3 temp_output_293_0 = (( maincolor232 * temp_output_536_0 )).rgb;
				float temp_output_61_0 = ( temp_output_536_0 * _Line1Alpha );
				float4 appendResult292 = (float4(temp_output_293_0 , temp_output_61_0));
				#ifdef _LINE1FEATURE_ON
				float4 staticSwitch284 = appendResult292;
				#else
				float4 staticSwitch284 = temp_cast_0;
				#endif
				float mulTime3_g168 = _TimeParameters.x * _Line2Speed;
				float2 temp_cast_2 = ((pos353*_Line2Frequency + ( mulTime3_g168 + randomoffset385 ))).xx;
				float clampResult42_g168 = clamp( ( ( tex2D( _Line2, temp_cast_2 ).r - _Line2InvertedThickness ) * _Line2Hardness ) , 0.0 , 1.0 );
				float temp_output_535_0 = clampResult42_g168;
				float3 temp_output_294_0 = (( maincolor232 * temp_output_535_0 )).rgb;
				float temp_output_69_0 = ( temp_output_535_0 * _Line2Alpha );
				float4 appendResult295 = (float4(temp_output_294_0 , temp_output_69_0));
				#ifdef _LINE2FEATURE_ON
				float4 staticSwitch285 = appendResult295;
				#else
				float4 staticSwitch285 = staticSwitch284;
				#endif
				float4 appendResult328 = (float4(0.0 , 0.0 , 0.0 , temp_output_61_0));
				float4 appendResult296 = (float4(( temp_output_293_0 * temp_output_294_0 ) , ( temp_output_61_0 * temp_output_69_0 )));
				#ifdef _LINEBOTHFEATURE_ON
				float4 staticSwitch286 = ( appendResult328 + appendResult296 );
				#else
				float4 staticSwitch286 = staticSwitch285;
				#endif
				float4 line_color298 = staticSwitch286;
				float4 temp_cast_4 = (C_ZERO287).xxxx;
				float4 screenPos238 = input.ase_texcoord5;
				float4 ase_positionSSNorm = screenPos238 / screenPos238.w;
				ase_positionSSNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_positionSSNorm.z : ase_positionSSNorm.z * 0.5 + 0.5;
				float screenDepth238 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth238 = saturate( abs( ( screenDepth238 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection1Distance ) ) );
				float saferPower344 = abs( distanceDepth238 );
				float lerpResult467 = lerp( 1.0 , pow( saferPower344 , _SoftIntersection1Intensity ) , _SoftIntersection1Affect);
				float4 appendResult321 = (float4(0.0 , 0.0 , 0.0 , lerpResult467));
				float temp_output_242_0 = ( 1.0 - distanceDepth238 );
				float4 appendResult314 = (float4((( maincolor232 * temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )).rgb , ( temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )));
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float4 staticSwitch315 = temp_cast_4;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float4 staticSwitch315 = appendResult321;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float4 staticSwitch315 = appendResult314;
				#else
				float4 staticSwitch315 = temp_cast_4;
				#endif
				float4 intersection1245 = staticSwitch315;
				float mulTime273 = _TimeParameters.x * 100.0;
				float simplePerlin3D264 = snoise( (ase_positionWS*_GrainScale + mulTime273) );
				simplePerlin3D264 = simplePerlin3D264*0.5 + 0.5;
				float lerpResult278 = lerp( _GrainValues.x , _GrainValues.y , simplePerlin3D264);
				float lerpResult269 = lerp( 0.0 , lerpResult278 , _GrainAffect);
				#ifdef _GRAINFEATURE_ON
				float staticSwitch282 = lerpResult269;
				#else
				float staticSwitch282 = 0.0;
				#endif
				float grain268 = staticSwitch282;
				float4 temp_cast_6 = (C_ZERO287).xxxx;
				float screenDepth417 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth417 = saturate( abs( ( screenDepth417 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection2Distance ) ) );
				float saferPower422 = abs( distanceDepth417 );
				float lerpResult471 = lerp( 1.0 , pow( saferPower422 , _SoftIntersection2Intensity ) , _SoftIntersection2Affect);
				float4 appendResult424 = (float4(0.0 , 0.0 , 0.0 , lerpResult471));
				float temp_output_418_0 = ( 1.0 - distanceDepth417 );
				float4 appendResult425 = (float4((( maincolor232 * temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )).rgb , ( temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )));
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float4 staticSwitch429 = temp_cast_6;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float4 staticSwitch429 = appendResult424;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float4 staticSwitch429 = appendResult425;
				#else
				float4 staticSwitch429 = temp_cast_6;
				#endif
				float4 intersection2430 = staticSwitch429;
				float fresnelalpha434 = clampResult57;
				float intersectionalpha1261 = (staticSwitch315).w;
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch330 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch330 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch330 = intersectionalpha1261;
				#else
				float staticSwitch330 = intersectionalpha1261;
				#endif
				float intersectionalpha2431 = (staticSwitch429).w;
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch447 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch447 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch447 = intersectionalpha2431;
				#else
				float staticSwitch447 = intersectionalpha2431;
				#endif
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch334 = C_ONE288;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch334 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch334 = C_ONE288;
				#else
				float staticSwitch334 = C_ONE288;
				#endif
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch448 = C_ONE288;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch448 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch448 = C_ONE288;
				#else
				float staticSwitch448 = C_ONE288;
				#endif
				float clampResult62 = clamp( ( ( (maincolor232).a + fresnelalpha434 + (line_color298).w + staticSwitch330 + staticSwitch447 ) * staticSwitch334 * staticSwitch448 ) , 0.0 , 1.0 );
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.ase_texcoord6.xyz , 0.0 ) ).xyz;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float3 staticSwitch375 = ase_positionWS;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float3 staticSwitch375 = input.ase_texcoord6.xyz;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float3 staticSwitch375 = temp_output_377_0;
				#else
				float3 staticSwitch375 = ase_positionWS;
				#endif
				float3 vPos357 = staticSwitch375;
				float simplePerlin3D212 = snoise( ( vPos357 * _DissolveScale ) );
				simplePerlin3D212 = simplePerlin3D212*0.5 + 0.5;
				float clampResult218 = clamp( ( simplePerlin3D212 - _DissolveHide ) , 0.0 , 1.0 );
				#ifdef _DISSOLVEFEATURE_ON
				float staticSwitch337 = clampResult218;
				#else
				float staticSwitch337 = C_ONE288;
				#endif
				float dissolve432 = staticSwitch337;
				float3 objToWorld257 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				#ifdef _MASKLOCALFEATURE_ON
				float3 staticSwitch499 = ( _MaskCenter + objToWorld257 );
				#else
				float3 staticSwitch499 = _MaskCenter;
				#endif
				float clampResult256 = clamp( ( distance( max( ( abs( ( ase_positionWS - staticSwitch499 ) ) - ( _MaskSize * float3( 0.5,0.5,0.5 ) ) ) , float3( 0,0,0 ) ) , float3( 0,0,0 ) ) / _MaskFalloff ) , 0.0 , 1.0 );
				float lerpResult501 = lerp( clampResult256 , ( 1.0 - clampResult256 ) , _MaskInversion);
				#ifdef _MASKFEATURE_ON
				float staticSwitch339 = lerpResult501;
				#else
				float staticSwitch339 = C_ONE288;
				#endif
				float mask254 = staticSwitch339;
				float4 _AlphaMask_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_AlphaMask_ST);
				float2 uv_AlphaMask = input.ase_texcoord2.xy * _AlphaMask_ST_Instance.xy + _AlphaMask_ST_Instance.zw;
				float lerpResult400 = lerp( C_ONE288 , tex2D( _AlphaMask, uv_AlphaMask ).r , _AlphaMaskAffect);
				#ifdef _ALPHAMASKFEATURE_ON
				float staticSwitch404 = lerpResult400;
				#else
				float staticSwitch404 = C_ONE288;
				#endif
				float alphamask402 = staticSwitch404;
				float4 appendResult325 = (float4((( staticSwitch279 * ( maincolor232 + fresnelcolor436 + float4( (line_color298).xyz , 0.0 ) + intersection1245 + grain268 + intersection2430 ) )).rgb , ( clampResult62 * dissolve432 * mask254 * alphamask402 * _Alpha )));
				float4 temp_output_504_0 = ( appendResult325 * input.ase_color );
				

				surfaceDescription.Alpha = (temp_output_504_0).w;
				surfaceDescription.AlphaClipThreshold = 0.5;

				#if _ALPHATEST_ON
					float alphaClipThreshold = 0.01f;
					#if ALPHA_CLIP_THRESHOLD
						alphaClipThreshold = surfaceDescription.AlphaClipThreshold;
					#endif
					clip(surfaceDescription.Alpha - alphaClipThreshold);
				#endif

				half4 outColor = half4(_ObjectId, _PassValue, 1.0, 1.0);
				return outColor;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "ScenePickingPass"
			Tags { "LightMode"="Picking" }

			AlphaToMask Off

			HLSLPROGRAM

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_VERSION 19800
			#define ASE_SRP_VERSION 170003
			#define REQUIRE_DEPTH_TEXTURE 1


			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT

			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRenderingKeywords.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRendering.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_POSITION
			#pragma shader_feature _VOXELIZATIONFEATURE_ON
			#pragma shader_feature _LINEGLITCHFEATURE_ON
			#pragma shader_feature _POSITIONSPACEFEATURE_WORLD _POSITIONSPACEFEATURE_LOCAL _POSITIONSPACEFEATURE_CUSTOM
			#pragma shader_feature _POSITIONFEATURE_X _POSITIONFEATURE_Y _POSITIONFEATURE_Z
			#pragma multi_compile_instancing
			#pragma shader_feature _RANDOMGLITCHFEATURE_ON
			#pragma shader_feature _COLORGLITCHFEATURE_ON
			#pragma shader_feature _FRESNELFEATURE_ON
			#pragma shader_feature _NORMALMAPFEATURE_ON
			#pragma shader_feature _LINEBOTHFEATURE_ON
			#pragma shader_feature _LINE2FEATURE_ON
			#pragma shader_feature _LINE1FEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION1FEATURE_OFF _SOFTINTERSECTION1FEATURE_ALPHA _SOFTINTERSECTION1FEATURE_COLOR
			#pragma shader_feature _GRAINFEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION2FEATURE_OFF _SOFTINTERSECTION2FEATURE_ALPHA _SOFTINTERSECTION2FEATURE_COLOR
			#pragma shader_feature _DISSOLVEFEATURE_ON
			#pragma shader_feature _MASKFEATURE_ON
			#pragma shader_feature _MASKLOCALFEATURE_ON
			#pragma shader_feature _ALPHAMASKFEATURE_ON


			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				float4 positionCS : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainColor;
			float3 _LineGlitchOffset;
			float3 _MaskSize;
			float3 _MaskCenter;
			float3 _DissolveScale;
			float3 _RandomGlitchOffset;
			float3 _GrainScale;
			float2 _GrainValues;
			float _CullMode;
			float _Line2Frequency;
			float _Line2Speed;
			float _Line2InvertedThickness;
			float _Line2Hardness;
			float _Line2Alpha;
			float _SoftIntersection1Distance;
			float _SoftIntersection1Intensity;
			float _GrainAffect;
			float _Line1Alpha;
			float _SoftIntersection2Distance;
			float _SoftIntersection2Intensity;
			float _SoftIntersection2Affect;
			float _DissolveHide;
			float _MaskFalloff;
			float _MaskInversion;
			float _SoftIntersection1Affect;
			float _Line1Hardness;
			float _Line1Speed;
			float _AlphaMaskAffect;
			float _ZWrite;
			float _PositionDirection;
			float _LineGlitchFrequency;
			float _LineGlitchSpeed;
			float _RandomOffset;
			float _LineGlitchInvertedThickness;
			float _LineGlitchHardness;
			float _RandomGlitchTiling;
			float _RandomGlitchConstant;
			float _Line1InvertedThickness;
			float _RandomGlitchAmount;
			float _VoxelizationAffect;
			float _ColorGlitchAffect;
			float _FresnelScale;
			float _FresnelPower;
			float _NormalScale;
			float _NormalAffect;
			float _FresnelAlphaScale;
			float _FresnelAlphaPower;
			float _Line1Frequency;
			float _Voxelization;
			float _Alpha;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _LineGlitch;
			sampler2D _NormalMap;
			sampler2D _Line1;
			sampler2D _Line2;
			sampler2D _AlphaMask;
			UNITY_INSTANCING_BUFFER_START(KnifeHologramShaderUnlit)
				UNITY_DEFINE_INSTANCED_PROP(float4x4, _CustomMatrix)
				UNITY_DEFINE_INSTANCED_PROP(float4, _NormalMap_ST)
				UNITY_DEFINE_INSTANCED_PROP(float4, _AlphaMask_ST)
			UNITY_INSTANCING_BUFFER_END(KnifeHologramShaderUnlit)


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float3 mod3D289( float3 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 mod3D289( float4 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 permute( float4 x ) { return mod3D289( ( x * 34.0 + 1.0 ) * x ); }
			float4 taylorInvSqrt( float4 r ) { return 1.79284291400159 - r * 0.85373472095314; }
			float snoise( float3 v )
			{
				const float2 C = float2( 1.0 / 6.0, 1.0 / 3.0 );
				float3 i = floor( v + dot( v, C.yyy ) );
				float3 x0 = v - i + dot( i, C.xxx );
				float3 g = step( x0.yzx, x0.xyz );
				float3 l = 1.0 - g;
				float3 i1 = min( g.xyz, l.zxy );
				float3 i2 = max( g.xyz, l.zxy );
				float3 x1 = x0 - i1 + C.xxx;
				float3 x2 = x0 - i2 + C.yyy;
				float3 x3 = x0 - 0.5;
				i = mod3D289( i);
				float4 p = permute( permute( permute( i.z + float4( 0.0, i1.z, i2.z, 1.0 ) ) + i.y + float4( 0.0, i1.y, i2.y, 1.0 ) ) + i.x + float4( 0.0, i1.x, i2.x, 1.0 ) );
				float4 j = p - 49.0 * floor( p / 49.0 );  // mod(p,7*7)
				float4 x_ = floor( j / 7.0 );
				float4 y_ = floor( j - 7.0 * x_ );  // mod(j,N)
				float4 x = ( x_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 y = ( y_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 h = 1.0 - abs( x ) - abs( y );
				float4 b0 = float4( x.xy, y.xy );
				float4 b1 = float4( x.zw, y.zw );
				float4 s0 = floor( b0 ) * 2.0 + 1.0;
				float4 s1 = floor( b1 ) * 2.0 + 1.0;
				float4 sh = -step( h, 0.0 );
				float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy;
				float4 a1 = b1.xzyw + s1.xzyw * sh.zzww;
				float3 g0 = float3( a0.xy, h.x );
				float3 g1 = float3( a0.zw, h.y );
				float3 g2 = float3( a1.xy, h.z );
				float3 g3 = float3( a1.zw, h.w );
				float4 norm = taylorInvSqrt( float4( dot( g0, g0 ), dot( g1, g1 ), dot( g2, g2 ), dot( g3, g3 ) ) );
				g0 *= norm.x;
				g1 *= norm.y;
				g2 *= norm.z;
				g3 *= norm.w;
				float4 m = max( 0.6 - float4( dot( x0, x0 ), dot( x1, x1 ), dot( x2, x2 ), dot( x3, x3 ) ), 0.0 );
				m = m* m;
				m = m* m;
				float4 px = float4( dot( x0, g0 ), dot( x1, g1 ), dot( x2, g2 ), dot( x3, g3 ) );
				return 42.0 * dot( m, px);
			}
			

			float4 _SelectionID;

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			PackedVaryings VertexFunction(Attributes input  )
			{
				PackedVaryings output;
				ZERO_INITIALIZE(PackedVaryings, output);

				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float C_ZERO287 = 0.0;
				float3 temp_cast_0 = (C_ZERO287).xxx;
				float3 viewToObjDir94 = mul( UNITY_MATRIX_T_MV, float4( _LineGlitchOffset, 0 ) ).xyz;
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float3 ase_positionWS = TransformObjectToWorld( (input.positionOS).xyz );
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch350 = ase_positionWS.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch350 = ase_positionWS.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch350 = ase_positionWS.z;
				#else
				float staticSwitch350 = ase_positionWS.y;
				#endif
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch351 = input.positionOS.xyz.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch351 = input.positionOS.xyz.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch351 = input.positionOS.xyz.z;
				#else
				float staticSwitch351 = input.positionOS.xyz.y;
				#endif
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.positionOS.xyz , 0.0 ) ).xyz;
				float3 break379 = temp_output_377_0;
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch378 = break379.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch378 = break379.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch378 = break379.z;
				#else
				float staticSwitch378 = break379.y;
				#endif
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch352 = staticSwitch350;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch352 = staticSwitch351;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch352 = staticSwitch378;
				#else
				float staticSwitch352 = staticSwitch350;
				#endif
				float vertexToFrag497 = ( staticSwitch352 * _PositionDirection );
				float pos353 = vertexToFrag497;
				float mulTime3_g167 = _TimeParameters.x * _LineGlitchSpeed;
				float randomoffset385 = _RandomOffset;
				float2 temp_cast_3 = ((pos353*_LineGlitchFrequency + ( mulTime3_g167 + randomoffset385 ))).xx;
				float clampResult42_g167 = clamp( ( ( tex2Dlod( _LineGlitch, float4( temp_cast_3, 0, 0.0) ).r - _LineGlitchInvertedThickness ) * _LineGlitchHardness ) , 0.0 , 1.0 );
				#ifdef _LINEGLITCHFEATURE_ON
				float3 staticSwitch307 = ( ( viewToObjDir94 / ase_objectScale ) * clampResult42_g167 );
				#else
				float3 staticSwitch307 = temp_cast_0;
				#endif
				float3 lineglitch491 = staticSwitch307;
				float3 temp_cast_4 = (C_ZERO287).xxx;
				float3 viewToObjDir122 = mul( UNITY_MATRIX_T_MV, float4( _RandomGlitchOffset, 0 ) ).xyz;
				float mulTime149 = _TimeParameters.x * -2.3;
				float mulTime155 = _TimeParameters.x * -2.05;
				float2 appendResult153 = (float2((pos353*_RandomGlitchTiling + ( mulTime149 + randomoffset385 )) , ( randomoffset385 + mulTime155 )));
				float simplePerlin2D186 = snoise( appendResult153 );
				simplePerlin2D186 = simplePerlin2D186*0.5 + 0.5;
				float4 matrixToPos373 = float4( float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[0][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[1][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[2][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[3][3]);
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch365 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch365 = 0.0;
				#else
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#endif
				float mulTime139 = _TimeParameters.x * -5.74;
				float mulTime157 = _TimeParameters.x * -0.83;
				float2 appendResult158 = (float2((staticSwitch365*223.0 + ( mulTime139 + randomoffset385 )) , ( randomoffset385 + mulTime157 )));
				float simplePerlin2D187 = snoise( appendResult158 );
				simplePerlin2D187 = simplePerlin2D187*0.5 + 0.5;
				float clampResult148 = clamp( (-1.0 + (( simplePerlin2D187 + _RandomGlitchConstant ) - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float temp_output_197_0 = ( (-1.0 + (simplePerlin2D186 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) * clampResult148 );
				float2 break190 = appendResult153;
				float2 appendResult195 = (float2(( 20.0 * break190.x ) , break190.y));
				float simplePerlin2D188 = snoise( appendResult195 );
				simplePerlin2D188 = simplePerlin2D188*0.5 + 0.5;
				float clampResult192 = clamp( (-1.0 + (simplePerlin2D188 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult199 = lerp( 0.0 , clampResult192 , 2.0);
				#ifdef _RANDOMGLITCHFEATURE_ON
				float3 staticSwitch311 = ( ( viewToObjDir122 / ase_objectScale ) * ( temp_output_197_0 + ( temp_output_197_0 * lerpResult199 ) ) * _RandomGlitchAmount );
				#else
				float3 staticSwitch311 = temp_cast_4;
				#endif
				float3 randomglitch493 = staticSwitch311;
				float3 vertexoffset410 = ( lineglitch491 + randomglitch493 );
				float3 temp_output_512_0 = ( input.positionOS.xyz + vertexoffset410 );
				float3 lerpResult517 = lerp( temp_output_512_0 , ( round( ( temp_output_512_0 * _Voxelization ) ) / _Voxelization ) , _VoxelizationAffect);
				#ifdef _VOXELIZATIONFEATURE_ON
				float3 staticSwitch518 = lerpResult517;
				#else
				float3 staticSwitch518 = temp_output_512_0;
				#endif
				float3 ModifiedVertexPosition519 = staticSwitch518;
				
				output.ase_texcoord.xyz = ase_positionWS;
				float3 ase_normalWS = TransformObjectToWorldNormal(input.normalOS);
				output.ase_texcoord1.xyz = ase_normalWS;
				float3 ase_tangentWS = TransformObjectToWorldDir(input.ase_tangent.xyz);
				output.ase_texcoord3.xyz = ase_tangentWS;
				float ase_tangentSign = input.ase_tangent.w * ( unity_WorldTransformParams.w >= 0.0 ? 1.0 : -1.0 );
				float3 ase_bitangentWS = cross( ase_normalWS, ase_tangentWS ) * ase_tangentSign;
				output.ase_texcoord4.xyz = ase_bitangentWS;
				output.ase_texcoord.w = vertexToFrag497;
				float3 vertexPos238 = ( input.positionOS.xyz + vertexoffset410 );
				float4 ase_positionCS238 = TransformObjectToHClip((vertexPos238).xyz);
				float4 screenPos238 = ComputeScreenPos(ase_positionCS238);
				output.ase_texcoord5 = screenPos238;
				float3 vertexPos417 = ( input.positionOS.xyz + vertexoffset410 );
				
				output.ase_texcoord2.xy = input.ase_texcoord.xy;
				output.ase_texcoord6 = input.positionOS;
				output.ase_color = input.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord1.w = 0;
				output.ase_texcoord2.zw = 0;
				output.ase_texcoord3.w = 0;
				output.ase_texcoord4.w = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ModifiedVertexPosition519;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				float3 positionWS = TransformObjectToWorld( input.positionOS.xyz );
				output.positionCS = TransformWorldToHClip(positionWS);
				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 positionOS : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.positionOS = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				output.ase_tangent = input.ase_tangent;
				output.ase_color = input.ase_color;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].positionOS, input[1].positionOS, input[2].positionOS, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].positionOS, input[1].positionOS, input[2].positionOS, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].positionOS, input[1].positionOS, input[2].positionOS, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].positionOS * bary.x + patch[1].positionOS * bary.y + patch[2].positionOS * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				output.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				output.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].positionOS.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			half4 frag(PackedVaryings input ) : SV_Target
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float3 objToWorld161 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				float C_ZERO287 = 0.0;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch360 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch360 = 0.0;
				#else
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#endif
				float randomoffset385 = _RandomOffset;
				float mulTime165 = _TimeParameters.x * -15.0;
				float mulTime167 = _TimeParameters.x * -0.5;
				float2 appendResult168 = (float2((staticSwitch360*223.0 + ( randomoffset385 + mulTime165 )) , ( randomoffset385 + mulTime167 )));
				float simplePerlin2D176 = snoise( appendResult168 );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float clampResult175 = clamp( (-0.61 + (simplePerlin2D176 - 0.0) * (2.0 - -0.61) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult181 = lerp( 1.0 , clampResult175 , _ColorGlitchAffect);
				#ifdef _COLORGLITCHFEATURE_ON
				float staticSwitch279 = lerpResult181;
				#else
				float staticSwitch279 = 1.0;
				#endif
				float4 maincolor232 = _MainColor;
				float C_ONE288 = 1.0;
				float3 ase_positionWS = input.ase_texcoord.xyz;
				float3 ase_viewVectorWS = ( _WorldSpaceCameraPos.xyz - ase_positionWS );
				float3 ase_viewDirWS = normalize( ase_viewVectorWS );
				float3 ase_normalWS = input.ase_texcoord1.xyz;
				float fresnelNdotV2 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode2 = ( 0.0 + _FresnelScale * pow( max( 1.0 - fresnelNdotV2 , 0.0001 ), _FresnelPower ) );
				float4 _NormalMap_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_NormalMap_ST);
				float2 uv_NormalMap = input.ase_texcoord2.xy * _NormalMap_ST_Instance.xy + _NormalMap_ST_Instance.zw;
				float3 unpack95 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalScale );
				unpack95.z = lerp( 1, unpack95.z, saturate(_NormalScale) );
				float3 ase_tangentWS = input.ase_texcoord3.xyz;
				float3 ase_bitangentWS = input.ase_texcoord4.xyz;
				float3 tanToWorld0 = float3( ase_tangentWS.x, ase_bitangentWS.x, ase_normalWS.x );
				float3 tanToWorld1 = float3( ase_tangentWS.y, ase_bitangentWS.y, ase_normalWS.y );
				float3 tanToWorld2 = float3( ase_tangentWS.z, ase_bitangentWS.z, ase_normalWS.z );
				float3 ase_viewVectorTS =  tanToWorld0 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).x + tanToWorld1 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).y  + tanToWorld2 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).z;
				float3 ase_viewDirTS = normalize( ase_viewVectorTS );
				float dotResult101 = dot( unpack95 , ase_viewDirTS );
				float lerpResult106 = lerp( 1.0 , (0.0 + (dotResult101 - -1.0) * (1.0 - 0.0) / (1.0 - -1.0)) , _NormalAffect);
				#ifdef _NORMALMAPFEATURE_ON
				float staticSwitch341 = ( 1.0 - lerpResult106 );
				#else
				float staticSwitch341 = C_ZERO287;
				#endif
				float NormalAffect116 = staticSwitch341;
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch303 = ( fresnelNode2 + NormalAffect116 );
				#else
				float staticSwitch303 = C_ONE288;
				#endif
				float fresnelNdotV53 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode53 = ( 0.0 + _FresnelAlphaScale * pow( max( 1.0 - fresnelNdotV53 , 0.0001 ), _FresnelAlphaPower ) );
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch305 = fresnelNode53;
				#else
				float staticSwitch305 = C_ZERO287;
				#endif
				float clampResult57 = clamp( ( staticSwitch305 + NormalAffect116 ) , 0.0 , 1.0 );
				float4 fresnelcolor436 = ( staticSwitch303 * maincolor232 * clampResult57 );
				float4 temp_cast_0 = (C_ZERO287).xxxx;
				float vertexToFrag497 = input.ase_texcoord.w;
				float pos353 = vertexToFrag497;
				float mulTime3_g169 = _TimeParameters.x * _Line1Speed;
				float2 temp_cast_1 = ((pos353*_Line1Frequency + ( mulTime3_g169 + randomoffset385 ))).xx;
				float clampResult42_g169 = clamp( ( ( tex2D( _Line1, temp_cast_1 ).r - _Line1InvertedThickness ) * _Line1Hardness ) , 0.0 , 1.0 );
				float temp_output_536_0 = clampResult42_g169;
				float3 temp_output_293_0 = (( maincolor232 * temp_output_536_0 )).rgb;
				float temp_output_61_0 = ( temp_output_536_0 * _Line1Alpha );
				float4 appendResult292 = (float4(temp_output_293_0 , temp_output_61_0));
				#ifdef _LINE1FEATURE_ON
				float4 staticSwitch284 = appendResult292;
				#else
				float4 staticSwitch284 = temp_cast_0;
				#endif
				float mulTime3_g168 = _TimeParameters.x * _Line2Speed;
				float2 temp_cast_2 = ((pos353*_Line2Frequency + ( mulTime3_g168 + randomoffset385 ))).xx;
				float clampResult42_g168 = clamp( ( ( tex2D( _Line2, temp_cast_2 ).r - _Line2InvertedThickness ) * _Line2Hardness ) , 0.0 , 1.0 );
				float temp_output_535_0 = clampResult42_g168;
				float3 temp_output_294_0 = (( maincolor232 * temp_output_535_0 )).rgb;
				float temp_output_69_0 = ( temp_output_535_0 * _Line2Alpha );
				float4 appendResult295 = (float4(temp_output_294_0 , temp_output_69_0));
				#ifdef _LINE2FEATURE_ON
				float4 staticSwitch285 = appendResult295;
				#else
				float4 staticSwitch285 = staticSwitch284;
				#endif
				float4 appendResult328 = (float4(0.0 , 0.0 , 0.0 , temp_output_61_0));
				float4 appendResult296 = (float4(( temp_output_293_0 * temp_output_294_0 ) , ( temp_output_61_0 * temp_output_69_0 )));
				#ifdef _LINEBOTHFEATURE_ON
				float4 staticSwitch286 = ( appendResult328 + appendResult296 );
				#else
				float4 staticSwitch286 = staticSwitch285;
				#endif
				float4 line_color298 = staticSwitch286;
				float4 temp_cast_4 = (C_ZERO287).xxxx;
				float4 screenPos238 = input.ase_texcoord5;
				float4 ase_positionSSNorm = screenPos238 / screenPos238.w;
				ase_positionSSNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_positionSSNorm.z : ase_positionSSNorm.z * 0.5 + 0.5;
				float screenDepth238 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth238 = saturate( abs( ( screenDepth238 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection1Distance ) ) );
				float saferPower344 = abs( distanceDepth238 );
				float lerpResult467 = lerp( 1.0 , pow( saferPower344 , _SoftIntersection1Intensity ) , _SoftIntersection1Affect);
				float4 appendResult321 = (float4(0.0 , 0.0 , 0.0 , lerpResult467));
				float temp_output_242_0 = ( 1.0 - distanceDepth238 );
				float4 appendResult314 = (float4((( maincolor232 * temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )).rgb , ( temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )));
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float4 staticSwitch315 = temp_cast_4;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float4 staticSwitch315 = appendResult321;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float4 staticSwitch315 = appendResult314;
				#else
				float4 staticSwitch315 = temp_cast_4;
				#endif
				float4 intersection1245 = staticSwitch315;
				float mulTime273 = _TimeParameters.x * 100.0;
				float simplePerlin3D264 = snoise( (ase_positionWS*_GrainScale + mulTime273) );
				simplePerlin3D264 = simplePerlin3D264*0.5 + 0.5;
				float lerpResult278 = lerp( _GrainValues.x , _GrainValues.y , simplePerlin3D264);
				float lerpResult269 = lerp( 0.0 , lerpResult278 , _GrainAffect);
				#ifdef _GRAINFEATURE_ON
				float staticSwitch282 = lerpResult269;
				#else
				float staticSwitch282 = 0.0;
				#endif
				float grain268 = staticSwitch282;
				float4 temp_cast_6 = (C_ZERO287).xxxx;
				float screenDepth417 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth417 = saturate( abs( ( screenDepth417 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection2Distance ) ) );
				float saferPower422 = abs( distanceDepth417 );
				float lerpResult471 = lerp( 1.0 , pow( saferPower422 , _SoftIntersection2Intensity ) , _SoftIntersection2Affect);
				float4 appendResult424 = (float4(0.0 , 0.0 , 0.0 , lerpResult471));
				float temp_output_418_0 = ( 1.0 - distanceDepth417 );
				float4 appendResult425 = (float4((( maincolor232 * temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )).rgb , ( temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )));
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float4 staticSwitch429 = temp_cast_6;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float4 staticSwitch429 = appendResult424;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float4 staticSwitch429 = appendResult425;
				#else
				float4 staticSwitch429 = temp_cast_6;
				#endif
				float4 intersection2430 = staticSwitch429;
				float fresnelalpha434 = clampResult57;
				float intersectionalpha1261 = (staticSwitch315).w;
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch330 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch330 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch330 = intersectionalpha1261;
				#else
				float staticSwitch330 = intersectionalpha1261;
				#endif
				float intersectionalpha2431 = (staticSwitch429).w;
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch447 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch447 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch447 = intersectionalpha2431;
				#else
				float staticSwitch447 = intersectionalpha2431;
				#endif
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch334 = C_ONE288;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch334 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch334 = C_ONE288;
				#else
				float staticSwitch334 = C_ONE288;
				#endif
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch448 = C_ONE288;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch448 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch448 = C_ONE288;
				#else
				float staticSwitch448 = C_ONE288;
				#endif
				float clampResult62 = clamp( ( ( (maincolor232).a + fresnelalpha434 + (line_color298).w + staticSwitch330 + staticSwitch447 ) * staticSwitch334 * staticSwitch448 ) , 0.0 , 1.0 );
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.ase_texcoord6.xyz , 0.0 ) ).xyz;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float3 staticSwitch375 = ase_positionWS;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float3 staticSwitch375 = input.ase_texcoord6.xyz;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float3 staticSwitch375 = temp_output_377_0;
				#else
				float3 staticSwitch375 = ase_positionWS;
				#endif
				float3 vPos357 = staticSwitch375;
				float simplePerlin3D212 = snoise( ( vPos357 * _DissolveScale ) );
				simplePerlin3D212 = simplePerlin3D212*0.5 + 0.5;
				float clampResult218 = clamp( ( simplePerlin3D212 - _DissolveHide ) , 0.0 , 1.0 );
				#ifdef _DISSOLVEFEATURE_ON
				float staticSwitch337 = clampResult218;
				#else
				float staticSwitch337 = C_ONE288;
				#endif
				float dissolve432 = staticSwitch337;
				float3 objToWorld257 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				#ifdef _MASKLOCALFEATURE_ON
				float3 staticSwitch499 = ( _MaskCenter + objToWorld257 );
				#else
				float3 staticSwitch499 = _MaskCenter;
				#endif
				float clampResult256 = clamp( ( distance( max( ( abs( ( ase_positionWS - staticSwitch499 ) ) - ( _MaskSize * float3( 0.5,0.5,0.5 ) ) ) , float3( 0,0,0 ) ) , float3( 0,0,0 ) ) / _MaskFalloff ) , 0.0 , 1.0 );
				float lerpResult501 = lerp( clampResult256 , ( 1.0 - clampResult256 ) , _MaskInversion);
				#ifdef _MASKFEATURE_ON
				float staticSwitch339 = lerpResult501;
				#else
				float staticSwitch339 = C_ONE288;
				#endif
				float mask254 = staticSwitch339;
				float4 _AlphaMask_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_AlphaMask_ST);
				float2 uv_AlphaMask = input.ase_texcoord2.xy * _AlphaMask_ST_Instance.xy + _AlphaMask_ST_Instance.zw;
				float lerpResult400 = lerp( C_ONE288 , tex2D( _AlphaMask, uv_AlphaMask ).r , _AlphaMaskAffect);
				#ifdef _ALPHAMASKFEATURE_ON
				float staticSwitch404 = lerpResult400;
				#else
				float staticSwitch404 = C_ONE288;
				#endif
				float alphamask402 = staticSwitch404;
				float4 appendResult325 = (float4((( staticSwitch279 * ( maincolor232 + fresnelcolor436 + float4( (line_color298).xyz , 0.0 ) + intersection1245 + grain268 + intersection2430 ) )).rgb , ( clampResult62 * dissolve432 * mask254 * alphamask402 * _Alpha )));
				float4 temp_output_504_0 = ( appendResult325 * input.ase_color );
				

				surfaceDescription.Alpha = (temp_output_504_0).w;
				surfaceDescription.AlphaClipThreshold = 0.5;

				#if _ALPHATEST_ON
					float alphaClipThreshold = 0.01f;
					#if ALPHA_CLIP_THRESHOLD
						alphaClipThreshold = surfaceDescription.AlphaClipThreshold;
					#endif
					clip(surfaceDescription.Alpha - alphaClipThreshold);
				#endif

				half4 outColor = 0;
				outColor = _SelectionID;

				return outColor;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthNormals"
			Tags { "LightMode"="DepthNormalsOnly" }

			ZTest LEqual
			ZWrite On

			HLSLPROGRAM

        	#define ASE_ABSOLUTE_VERTEX_POS 1
        	#define _SURFACE_TYPE_TRANSPARENT 1
        	#define ASE_VERSION 19800
        	#define ASE_SRP_VERSION 170003
        	#define REQUIRE_DEPTH_TEXTURE 1


        	#pragma multi_compile_fragment _ _GBUFFER_NORMALS_OCT

			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define VARYINGS_NEED_NORMAL_WS

			#define SHADERPASS SHADERPASS_DEPTHNORMALSONLY

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRenderingKeywords.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRendering.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

            #if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_WORLD_NORMAL
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_POSITION
			#pragma shader_feature _VOXELIZATIONFEATURE_ON
			#pragma shader_feature _LINEGLITCHFEATURE_ON
			#pragma shader_feature _POSITIONSPACEFEATURE_WORLD _POSITIONSPACEFEATURE_LOCAL _POSITIONSPACEFEATURE_CUSTOM
			#pragma shader_feature _POSITIONFEATURE_X _POSITIONFEATURE_Y _POSITIONFEATURE_Z
			#pragma multi_compile_instancing
			#pragma shader_feature _RANDOMGLITCHFEATURE_ON
			#pragma shader_feature _COLORGLITCHFEATURE_ON
			#pragma shader_feature _FRESNELFEATURE_ON
			#pragma shader_feature _NORMALMAPFEATURE_ON
			#pragma shader_feature _LINEBOTHFEATURE_ON
			#pragma shader_feature _LINE2FEATURE_ON
			#pragma shader_feature _LINE1FEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION1FEATURE_OFF _SOFTINTERSECTION1FEATURE_ALPHA _SOFTINTERSECTION1FEATURE_COLOR
			#pragma shader_feature _GRAINFEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION2FEATURE_OFF _SOFTINTERSECTION2FEATURE_ALPHA _SOFTINTERSECTION2FEATURE_COLOR
			#pragma shader_feature _DISSOLVEFEATURE_ON
			#pragma shader_feature _MASKFEATURE_ON
			#pragma shader_feature _MASKLOCALFEATURE_ON
			#pragma shader_feature _ALPHAMASKFEATURE_ON


			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE) && (SHADER_TARGET >= 45)
				#define ASE_SV_DEPTH SV_DepthLessEqual
				#define ASE_SV_POSITION_QUALIFIERS linear noperspective centroid
			#else
				#define ASE_SV_DEPTH SV_Depth
				#define ASE_SV_POSITION_QUALIFIERS
			#endif

			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				ASE_SV_POSITION_QUALIFIERS float4 positionCS : SV_POSITION;
				float4 clipPosV : TEXCOORD0;
				float3 positionWS : TEXCOORD1;
				float3 normalWS : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainColor;
			float3 _LineGlitchOffset;
			float3 _MaskSize;
			float3 _MaskCenter;
			float3 _DissolveScale;
			float3 _RandomGlitchOffset;
			float3 _GrainScale;
			float2 _GrainValues;
			float _CullMode;
			float _Line2Frequency;
			float _Line2Speed;
			float _Line2InvertedThickness;
			float _Line2Hardness;
			float _Line2Alpha;
			float _SoftIntersection1Distance;
			float _SoftIntersection1Intensity;
			float _GrainAffect;
			float _Line1Alpha;
			float _SoftIntersection2Distance;
			float _SoftIntersection2Intensity;
			float _SoftIntersection2Affect;
			float _DissolveHide;
			float _MaskFalloff;
			float _MaskInversion;
			float _SoftIntersection1Affect;
			float _Line1Hardness;
			float _Line1Speed;
			float _AlphaMaskAffect;
			float _ZWrite;
			float _PositionDirection;
			float _LineGlitchFrequency;
			float _LineGlitchSpeed;
			float _RandomOffset;
			float _LineGlitchInvertedThickness;
			float _LineGlitchHardness;
			float _RandomGlitchTiling;
			float _RandomGlitchConstant;
			float _Line1InvertedThickness;
			float _RandomGlitchAmount;
			float _VoxelizationAffect;
			float _ColorGlitchAffect;
			float _FresnelScale;
			float _FresnelPower;
			float _NormalScale;
			float _NormalAffect;
			float _FresnelAlphaScale;
			float _FresnelAlphaPower;
			float _Line1Frequency;
			float _Voxelization;
			float _Alpha;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _LineGlitch;
			sampler2D _NormalMap;
			sampler2D _Line1;
			sampler2D _Line2;
			sampler2D _AlphaMask;
			UNITY_INSTANCING_BUFFER_START(KnifeHologramShaderUnlit)
				UNITY_DEFINE_INSTANCED_PROP(float4x4, _CustomMatrix)
				UNITY_DEFINE_INSTANCED_PROP(float4, _NormalMap_ST)
				UNITY_DEFINE_INSTANCED_PROP(float4, _AlphaMask_ST)
			UNITY_INSTANCING_BUFFER_END(KnifeHologramShaderUnlit)


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float3 mod3D289( float3 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 mod3D289( float4 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 permute( float4 x ) { return mod3D289( ( x * 34.0 + 1.0 ) * x ); }
			float4 taylorInvSqrt( float4 r ) { return 1.79284291400159 - r * 0.85373472095314; }
			float snoise( float3 v )
			{
				const float2 C = float2( 1.0 / 6.0, 1.0 / 3.0 );
				float3 i = floor( v + dot( v, C.yyy ) );
				float3 x0 = v - i + dot( i, C.xxx );
				float3 g = step( x0.yzx, x0.xyz );
				float3 l = 1.0 - g;
				float3 i1 = min( g.xyz, l.zxy );
				float3 i2 = max( g.xyz, l.zxy );
				float3 x1 = x0 - i1 + C.xxx;
				float3 x2 = x0 - i2 + C.yyy;
				float3 x3 = x0 - 0.5;
				i = mod3D289( i);
				float4 p = permute( permute( permute( i.z + float4( 0.0, i1.z, i2.z, 1.0 ) ) + i.y + float4( 0.0, i1.y, i2.y, 1.0 ) ) + i.x + float4( 0.0, i1.x, i2.x, 1.0 ) );
				float4 j = p - 49.0 * floor( p / 49.0 );  // mod(p,7*7)
				float4 x_ = floor( j / 7.0 );
				float4 y_ = floor( j - 7.0 * x_ );  // mod(j,N)
				float4 x = ( x_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 y = ( y_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 h = 1.0 - abs( x ) - abs( y );
				float4 b0 = float4( x.xy, y.xy );
				float4 b1 = float4( x.zw, y.zw );
				float4 s0 = floor( b0 ) * 2.0 + 1.0;
				float4 s1 = floor( b1 ) * 2.0 + 1.0;
				float4 sh = -step( h, 0.0 );
				float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy;
				float4 a1 = b1.xzyw + s1.xzyw * sh.zzww;
				float3 g0 = float3( a0.xy, h.x );
				float3 g1 = float3( a0.zw, h.y );
				float3 g2 = float3( a1.xy, h.z );
				float3 g3 = float3( a1.zw, h.w );
				float4 norm = taylorInvSqrt( float4( dot( g0, g0 ), dot( g1, g1 ), dot( g2, g2 ), dot( g3, g3 ) ) );
				g0 *= norm.x;
				g1 *= norm.y;
				g2 *= norm.z;
				g3 *= norm.w;
				float4 m = max( 0.6 - float4( dot( x0, x0 ), dot( x1, x1 ), dot( x2, x2 ), dot( x3, x3 ) ), 0.0 );
				m = m* m;
				m = m* m;
				float4 px = float4( dot( x0, g0 ), dot( x1, g1 ), dot( x2, g2 ), dot( x3, g3 ) );
				return 42.0 * dot( m, px);
			}
			

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			PackedVaryings VertexFunction( Attributes input  )
			{
				PackedVaryings output;
				ZERO_INITIALIZE(PackedVaryings, output);

				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float C_ZERO287 = 0.0;
				float3 temp_cast_0 = (C_ZERO287).xxx;
				float3 viewToObjDir94 = mul( UNITY_MATRIX_T_MV, float4( _LineGlitchOffset, 0 ) ).xyz;
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float3 ase_positionWS = TransformObjectToWorld( (input.positionOS).xyz );
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch350 = ase_positionWS.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch350 = ase_positionWS.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch350 = ase_positionWS.z;
				#else
				float staticSwitch350 = ase_positionWS.y;
				#endif
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch351 = input.positionOS.xyz.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch351 = input.positionOS.xyz.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch351 = input.positionOS.xyz.z;
				#else
				float staticSwitch351 = input.positionOS.xyz.y;
				#endif
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.positionOS.xyz , 0.0 ) ).xyz;
				float3 break379 = temp_output_377_0;
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch378 = break379.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch378 = break379.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch378 = break379.z;
				#else
				float staticSwitch378 = break379.y;
				#endif
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch352 = staticSwitch350;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch352 = staticSwitch351;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch352 = staticSwitch378;
				#else
				float staticSwitch352 = staticSwitch350;
				#endif
				float vertexToFrag497 = ( staticSwitch352 * _PositionDirection );
				float pos353 = vertexToFrag497;
				float mulTime3_g167 = _TimeParameters.x * _LineGlitchSpeed;
				float randomoffset385 = _RandomOffset;
				float2 temp_cast_3 = ((pos353*_LineGlitchFrequency + ( mulTime3_g167 + randomoffset385 ))).xx;
				float clampResult42_g167 = clamp( ( ( tex2Dlod( _LineGlitch, float4( temp_cast_3, 0, 0.0) ).r - _LineGlitchInvertedThickness ) * _LineGlitchHardness ) , 0.0 , 1.0 );
				#ifdef _LINEGLITCHFEATURE_ON
				float3 staticSwitch307 = ( ( viewToObjDir94 / ase_objectScale ) * clampResult42_g167 );
				#else
				float3 staticSwitch307 = temp_cast_0;
				#endif
				float3 lineglitch491 = staticSwitch307;
				float3 temp_cast_4 = (C_ZERO287).xxx;
				float3 viewToObjDir122 = mul( UNITY_MATRIX_T_MV, float4( _RandomGlitchOffset, 0 ) ).xyz;
				float mulTime149 = _TimeParameters.x * -2.3;
				float mulTime155 = _TimeParameters.x * -2.05;
				float2 appendResult153 = (float2((pos353*_RandomGlitchTiling + ( mulTime149 + randomoffset385 )) , ( randomoffset385 + mulTime155 )));
				float simplePerlin2D186 = snoise( appendResult153 );
				simplePerlin2D186 = simplePerlin2D186*0.5 + 0.5;
				float4 matrixToPos373 = float4( float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[0][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[1][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[2][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[3][3]);
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch365 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch365 = 0.0;
				#else
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#endif
				float mulTime139 = _TimeParameters.x * -5.74;
				float mulTime157 = _TimeParameters.x * -0.83;
				float2 appendResult158 = (float2((staticSwitch365*223.0 + ( mulTime139 + randomoffset385 )) , ( randomoffset385 + mulTime157 )));
				float simplePerlin2D187 = snoise( appendResult158 );
				simplePerlin2D187 = simplePerlin2D187*0.5 + 0.5;
				float clampResult148 = clamp( (-1.0 + (( simplePerlin2D187 + _RandomGlitchConstant ) - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float temp_output_197_0 = ( (-1.0 + (simplePerlin2D186 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) * clampResult148 );
				float2 break190 = appendResult153;
				float2 appendResult195 = (float2(( 20.0 * break190.x ) , break190.y));
				float simplePerlin2D188 = snoise( appendResult195 );
				simplePerlin2D188 = simplePerlin2D188*0.5 + 0.5;
				float clampResult192 = clamp( (-1.0 + (simplePerlin2D188 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult199 = lerp( 0.0 , clampResult192 , 2.0);
				#ifdef _RANDOMGLITCHFEATURE_ON
				float3 staticSwitch311 = ( ( viewToObjDir122 / ase_objectScale ) * ( temp_output_197_0 + ( temp_output_197_0 * lerpResult199 ) ) * _RandomGlitchAmount );
				#else
				float3 staticSwitch311 = temp_cast_4;
				#endif
				float3 randomglitch493 = staticSwitch311;
				float3 vertexoffset410 = ( lineglitch491 + randomglitch493 );
				float3 temp_output_512_0 = ( input.positionOS.xyz + vertexoffset410 );
				float3 lerpResult517 = lerp( temp_output_512_0 , ( round( ( temp_output_512_0 * _Voxelization ) ) / _Voxelization ) , _VoxelizationAffect);
				#ifdef _VOXELIZATIONFEATURE_ON
				float3 staticSwitch518 = lerpResult517;
				#else
				float3 staticSwitch518 = temp_output_512_0;
				#endif
				float3 ModifiedVertexPosition519 = staticSwitch518;
				
				float3 ase_tangentWS = TransformObjectToWorldDir(input.ase_tangent.xyz);
				output.ase_texcoord4.xyz = ase_tangentWS;
				float3 ase_normalWS = TransformObjectToWorldNormal(input.normalOS);
				float ase_tangentSign = input.ase_tangent.w * ( unity_WorldTransformParams.w >= 0.0 ? 1.0 : -1.0 );
				float3 ase_bitangentWS = cross( ase_normalWS, ase_tangentWS ) * ase_tangentSign;
				output.ase_texcoord5.xyz = ase_bitangentWS;
				output.ase_texcoord3.z = vertexToFrag497;
				float3 vertexPos238 = ( input.positionOS.xyz + vertexoffset410 );
				float4 ase_positionCS238 = TransformObjectToHClip((vertexPos238).xyz);
				float4 screenPos238 = ComputeScreenPos(ase_positionCS238);
				output.ase_texcoord6 = screenPos238;
				float3 vertexPos417 = ( input.positionOS.xyz + vertexoffset410 );
				
				output.ase_texcoord3.xy = input.ase_texcoord.xy;
				output.ase_texcoord7 = input.positionOS;
				output.ase_color = input.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord3.w = 0;
				output.ase_texcoord4.w = 0;
				output.ase_texcoord5.w = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ModifiedVertexPosition519;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				VertexPositionInputs vertexInput = GetVertexPositionInputs( input.positionOS.xyz );

				output.positionCS = vertexInput.positionCS;
				output.clipPosV = vertexInput.positionCS;
				output.positionWS = vertexInput.positionWS;
				output.normalWS = TransformObjectToWorldNormal( input.normalOS );
				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 positionOS : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.positionOS = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				output.ase_tangent = input.ase_tangent;
				output.ase_color = input.ase_color;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].positionOS, input[1].positionOS, input[2].positionOS, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].positionOS, input[1].positionOS, input[2].positionOS, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].positionOS, input[1].positionOS, input[2].positionOS, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].positionOS * bary.x + patch[1].positionOS * bary.y + patch[2].positionOS * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				output.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				output.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].positionOS.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			void frag(PackedVaryings input
						, out half4 outNormalWS : SV_Target0
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						#ifdef _WRITE_RENDERING_LAYERS
						, out float4 outRenderingLayers : SV_Target1
						#endif
						 )
			{
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( input );
				float3 WorldPosition = input.positionWS;
				float3 WorldNormal = input.normalWS;
				float4 ClipPos = input.clipPosV;
				float4 ScreenPos = ComputeScreenPos( input.clipPosV );

				float3 objToWorld161 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				float C_ZERO287 = 0.0;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch360 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch360 = 0.0;
				#else
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#endif
				float randomoffset385 = _RandomOffset;
				float mulTime165 = _TimeParameters.x * -15.0;
				float mulTime167 = _TimeParameters.x * -0.5;
				float2 appendResult168 = (float2((staticSwitch360*223.0 + ( randomoffset385 + mulTime165 )) , ( randomoffset385 + mulTime167 )));
				float simplePerlin2D176 = snoise( appendResult168 );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float clampResult175 = clamp( (-0.61 + (simplePerlin2D176 - 0.0) * (2.0 - -0.61) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult181 = lerp( 1.0 , clampResult175 , _ColorGlitchAffect);
				#ifdef _COLORGLITCHFEATURE_ON
				float staticSwitch279 = lerpResult181;
				#else
				float staticSwitch279 = 1.0;
				#endif
				float4 maincolor232 = _MainColor;
				float C_ONE288 = 1.0;
				float3 ase_viewVectorWS = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				float3 ase_viewDirWS = normalize( ase_viewVectorWS );
				float fresnelNdotV2 = dot( WorldNormal, ase_viewDirWS );
				float fresnelNode2 = ( 0.0 + _FresnelScale * pow( max( 1.0 - fresnelNdotV2 , 0.0001 ), _FresnelPower ) );
				float4 _NormalMap_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_NormalMap_ST);
				float2 uv_NormalMap = input.ase_texcoord3.xy * _NormalMap_ST_Instance.xy + _NormalMap_ST_Instance.zw;
				float3 unpack95 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalScale );
				unpack95.z = lerp( 1, unpack95.z, saturate(_NormalScale) );
				float3 ase_tangentWS = input.ase_texcoord4.xyz;
				float3 ase_bitangentWS = input.ase_texcoord5.xyz;
				float3 tanToWorld0 = float3( ase_tangentWS.x, ase_bitangentWS.x, WorldNormal.x );
				float3 tanToWorld1 = float3( ase_tangentWS.y, ase_bitangentWS.y, WorldNormal.y );
				float3 tanToWorld2 = float3( ase_tangentWS.z, ase_bitangentWS.z, WorldNormal.z );
				float3 ase_viewVectorTS =  tanToWorld0 * ( _WorldSpaceCameraPos.xyz - WorldPosition ).x + tanToWorld1 * ( _WorldSpaceCameraPos.xyz - WorldPosition ).y  + tanToWorld2 * ( _WorldSpaceCameraPos.xyz - WorldPosition ).z;
				float3 ase_viewDirTS = normalize( ase_viewVectorTS );
				float dotResult101 = dot( unpack95 , ase_viewDirTS );
				float lerpResult106 = lerp( 1.0 , (0.0 + (dotResult101 - -1.0) * (1.0 - 0.0) / (1.0 - -1.0)) , _NormalAffect);
				#ifdef _NORMALMAPFEATURE_ON
				float staticSwitch341 = ( 1.0 - lerpResult106 );
				#else
				float staticSwitch341 = C_ZERO287;
				#endif
				float NormalAffect116 = staticSwitch341;
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch303 = ( fresnelNode2 + NormalAffect116 );
				#else
				float staticSwitch303 = C_ONE288;
				#endif
				float fresnelNdotV53 = dot( WorldNormal, ase_viewDirWS );
				float fresnelNode53 = ( 0.0 + _FresnelAlphaScale * pow( max( 1.0 - fresnelNdotV53 , 0.0001 ), _FresnelAlphaPower ) );
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch305 = fresnelNode53;
				#else
				float staticSwitch305 = C_ZERO287;
				#endif
				float clampResult57 = clamp( ( staticSwitch305 + NormalAffect116 ) , 0.0 , 1.0 );
				float4 fresnelcolor436 = ( staticSwitch303 * maincolor232 * clampResult57 );
				float4 temp_cast_0 = (C_ZERO287).xxxx;
				float vertexToFrag497 = input.ase_texcoord3.z;
				float pos353 = vertexToFrag497;
				float mulTime3_g169 = _TimeParameters.x * _Line1Speed;
				float2 temp_cast_1 = ((pos353*_Line1Frequency + ( mulTime3_g169 + randomoffset385 ))).xx;
				float clampResult42_g169 = clamp( ( ( tex2D( _Line1, temp_cast_1 ).r - _Line1InvertedThickness ) * _Line1Hardness ) , 0.0 , 1.0 );
				float temp_output_536_0 = clampResult42_g169;
				float3 temp_output_293_0 = (( maincolor232 * temp_output_536_0 )).rgb;
				float temp_output_61_0 = ( temp_output_536_0 * _Line1Alpha );
				float4 appendResult292 = (float4(temp_output_293_0 , temp_output_61_0));
				#ifdef _LINE1FEATURE_ON
				float4 staticSwitch284 = appendResult292;
				#else
				float4 staticSwitch284 = temp_cast_0;
				#endif
				float mulTime3_g168 = _TimeParameters.x * _Line2Speed;
				float2 temp_cast_2 = ((pos353*_Line2Frequency + ( mulTime3_g168 + randomoffset385 ))).xx;
				float clampResult42_g168 = clamp( ( ( tex2D( _Line2, temp_cast_2 ).r - _Line2InvertedThickness ) * _Line2Hardness ) , 0.0 , 1.0 );
				float temp_output_535_0 = clampResult42_g168;
				float3 temp_output_294_0 = (( maincolor232 * temp_output_535_0 )).rgb;
				float temp_output_69_0 = ( temp_output_535_0 * _Line2Alpha );
				float4 appendResult295 = (float4(temp_output_294_0 , temp_output_69_0));
				#ifdef _LINE2FEATURE_ON
				float4 staticSwitch285 = appendResult295;
				#else
				float4 staticSwitch285 = staticSwitch284;
				#endif
				float4 appendResult328 = (float4(0.0 , 0.0 , 0.0 , temp_output_61_0));
				float4 appendResult296 = (float4(( temp_output_293_0 * temp_output_294_0 ) , ( temp_output_61_0 * temp_output_69_0 )));
				#ifdef _LINEBOTHFEATURE_ON
				float4 staticSwitch286 = ( appendResult328 + appendResult296 );
				#else
				float4 staticSwitch286 = staticSwitch285;
				#endif
				float4 line_color298 = staticSwitch286;
				float4 temp_cast_4 = (C_ZERO287).xxxx;
				float4 screenPos238 = input.ase_texcoord6;
				float4 ase_positionSSNorm = screenPos238 / screenPos238.w;
				ase_positionSSNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_positionSSNorm.z : ase_positionSSNorm.z * 0.5 + 0.5;
				float screenDepth238 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth238 = saturate( abs( ( screenDepth238 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection1Distance ) ) );
				float saferPower344 = abs( distanceDepth238 );
				float lerpResult467 = lerp( 1.0 , pow( saferPower344 , _SoftIntersection1Intensity ) , _SoftIntersection1Affect);
				float4 appendResult321 = (float4(0.0 , 0.0 , 0.0 , lerpResult467));
				float temp_output_242_0 = ( 1.0 - distanceDepth238 );
				float4 appendResult314 = (float4((( maincolor232 * temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )).rgb , ( temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )));
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float4 staticSwitch315 = temp_cast_4;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float4 staticSwitch315 = appendResult321;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float4 staticSwitch315 = appendResult314;
				#else
				float4 staticSwitch315 = temp_cast_4;
				#endif
				float4 intersection1245 = staticSwitch315;
				float mulTime273 = _TimeParameters.x * 100.0;
				float simplePerlin3D264 = snoise( (WorldPosition*_GrainScale + mulTime273) );
				simplePerlin3D264 = simplePerlin3D264*0.5 + 0.5;
				float lerpResult278 = lerp( _GrainValues.x , _GrainValues.y , simplePerlin3D264);
				float lerpResult269 = lerp( 0.0 , lerpResult278 , _GrainAffect);
				#ifdef _GRAINFEATURE_ON
				float staticSwitch282 = lerpResult269;
				#else
				float staticSwitch282 = 0.0;
				#endif
				float grain268 = staticSwitch282;
				float4 temp_cast_6 = (C_ZERO287).xxxx;
				float screenDepth417 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth417 = saturate( abs( ( screenDepth417 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection2Distance ) ) );
				float saferPower422 = abs( distanceDepth417 );
				float lerpResult471 = lerp( 1.0 , pow( saferPower422 , _SoftIntersection2Intensity ) , _SoftIntersection2Affect);
				float4 appendResult424 = (float4(0.0 , 0.0 , 0.0 , lerpResult471));
				float temp_output_418_0 = ( 1.0 - distanceDepth417 );
				float4 appendResult425 = (float4((( maincolor232 * temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )).rgb , ( temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )));
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float4 staticSwitch429 = temp_cast_6;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float4 staticSwitch429 = appendResult424;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float4 staticSwitch429 = appendResult425;
				#else
				float4 staticSwitch429 = temp_cast_6;
				#endif
				float4 intersection2430 = staticSwitch429;
				float fresnelalpha434 = clampResult57;
				float intersectionalpha1261 = (staticSwitch315).w;
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch330 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch330 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch330 = intersectionalpha1261;
				#else
				float staticSwitch330 = intersectionalpha1261;
				#endif
				float intersectionalpha2431 = (staticSwitch429).w;
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch447 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch447 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch447 = intersectionalpha2431;
				#else
				float staticSwitch447 = intersectionalpha2431;
				#endif
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch334 = C_ONE288;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch334 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch334 = C_ONE288;
				#else
				float staticSwitch334 = C_ONE288;
				#endif
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch448 = C_ONE288;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch448 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch448 = C_ONE288;
				#else
				float staticSwitch448 = C_ONE288;
				#endif
				float clampResult62 = clamp( ( ( (maincolor232).a + fresnelalpha434 + (line_color298).w + staticSwitch330 + staticSwitch447 ) * staticSwitch334 * staticSwitch448 ) , 0.0 , 1.0 );
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.ase_texcoord7.xyz , 0.0 ) ).xyz;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float3 staticSwitch375 = WorldPosition;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float3 staticSwitch375 = input.ase_texcoord7.xyz;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float3 staticSwitch375 = temp_output_377_0;
				#else
				float3 staticSwitch375 = WorldPosition;
				#endif
				float3 vPos357 = staticSwitch375;
				float simplePerlin3D212 = snoise( ( vPos357 * _DissolveScale ) );
				simplePerlin3D212 = simplePerlin3D212*0.5 + 0.5;
				float clampResult218 = clamp( ( simplePerlin3D212 - _DissolveHide ) , 0.0 , 1.0 );
				#ifdef _DISSOLVEFEATURE_ON
				float staticSwitch337 = clampResult218;
				#else
				float staticSwitch337 = C_ONE288;
				#endif
				float dissolve432 = staticSwitch337;
				float3 objToWorld257 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				#ifdef _MASKLOCALFEATURE_ON
				float3 staticSwitch499 = ( _MaskCenter + objToWorld257 );
				#else
				float3 staticSwitch499 = _MaskCenter;
				#endif
				float clampResult256 = clamp( ( distance( max( ( abs( ( WorldPosition - staticSwitch499 ) ) - ( _MaskSize * float3( 0.5,0.5,0.5 ) ) ) , float3( 0,0,0 ) ) , float3( 0,0,0 ) ) / _MaskFalloff ) , 0.0 , 1.0 );
				float lerpResult501 = lerp( clampResult256 , ( 1.0 - clampResult256 ) , _MaskInversion);
				#ifdef _MASKFEATURE_ON
				float staticSwitch339 = lerpResult501;
				#else
				float staticSwitch339 = C_ONE288;
				#endif
				float mask254 = staticSwitch339;
				float4 _AlphaMask_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_AlphaMask_ST);
				float2 uv_AlphaMask = input.ase_texcoord3.xy * _AlphaMask_ST_Instance.xy + _AlphaMask_ST_Instance.zw;
				float lerpResult400 = lerp( C_ONE288 , tex2D( _AlphaMask, uv_AlphaMask ).r , _AlphaMaskAffect);
				#ifdef _ALPHAMASKFEATURE_ON
				float staticSwitch404 = lerpResult400;
				#else
				float staticSwitch404 = C_ONE288;
				#endif
				float alphamask402 = staticSwitch404;
				float4 appendResult325 = (float4((( staticSwitch279 * ( maincolor232 + fresnelcolor436 + float4( (line_color298).xyz , 0.0 ) + intersection1245 + grain268 + intersection2430 ) )).rgb , ( clampResult62 * dissolve432 * mask254 * alphamask402 * _Alpha )));
				float4 temp_output_504_0 = ( appendResult325 * input.ase_color );
				

				float Alpha = (temp_output_504_0).w;
				float AlphaClipThreshold = 0.5;

				#ifdef ASE_DEPTH_WRITE_ON
					float DepthValue = input.positionCS.z;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#if defined(LOD_FADE_CROSSFADE)
					LODFadeCrossFade( input.positionCS );
				#endif

				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif

				#if defined(_GBUFFER_NORMALS_OCT)
					float3 normalWS = normalize(input.normalWS);
					float2 octNormalWS = PackNormalOctQuadEncode(normalWS);
					float2 remappedOctNormalWS = saturate(octNormalWS * 0.5 + 0.5);
					half3 packedNormalWS = PackFloat2To888(remappedOctNormalWS);
					outNormalWS = half4(packedNormalWS, 0.0);
				#else
					float3 normalWS = input.normalWS;
					outNormalWS = half4(NormalizeNormalPerPixel(normalWS), 0.0);
				#endif

				#ifdef _WRITE_RENDERING_LAYERS
					uint renderingLayers = GetMeshRenderingLayer();
					outRenderingLayers = float4(EncodeMeshRenderingLayer(renderingLayers), 0, 0, 0);
				#endif
			}
			ENDHLSL
		}
		
		
		Pass
		{
			
			Name "MotionVectors"
			Tags { "LightMode"="MotionVectors" }

			ColorMask RG

			HLSLPROGRAM

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_VERSION 19800
			#define ASE_SRP_VERSION 170003
			#define REQUIRE_DEPTH_TEXTURE 1


			#pragma vertex vert
			#pragma fragment frag

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
		    #include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
		    #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
		    #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
		    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
		    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
		    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Input.hlsl"
		    #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRenderingKeywords.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRendering.hlsl"
		    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
		    #include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MotionVectorsCommon.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_POSITION
			#pragma shader_feature _VOXELIZATIONFEATURE_ON
			#pragma shader_feature _LINEGLITCHFEATURE_ON
			#pragma shader_feature _POSITIONSPACEFEATURE_WORLD _POSITIONSPACEFEATURE_LOCAL _POSITIONSPACEFEATURE_CUSTOM
			#pragma shader_feature _POSITIONFEATURE_X _POSITIONFEATURE_Y _POSITIONFEATURE_Z
			#pragma multi_compile_instancing
			#pragma shader_feature _RANDOMGLITCHFEATURE_ON
			#pragma shader_feature _COLORGLITCHFEATURE_ON
			#pragma shader_feature _FRESNELFEATURE_ON
			#pragma shader_feature _NORMALMAPFEATURE_ON
			#pragma shader_feature _LINEBOTHFEATURE_ON
			#pragma shader_feature _LINE2FEATURE_ON
			#pragma shader_feature _LINE1FEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION1FEATURE_OFF _SOFTINTERSECTION1FEATURE_ALPHA _SOFTINTERSECTION1FEATURE_COLOR
			#pragma shader_feature _GRAINFEATURE_ON
			#pragma shader_feature _SOFTINTERSECTION2FEATURE_OFF _SOFTINTERSECTION2FEATURE_ALPHA _SOFTINTERSECTION2FEATURE_COLOR
			#pragma shader_feature _DISSOLVEFEATURE_ON
			#pragma shader_feature _MASKFEATURE_ON
			#pragma shader_feature _MASKLOCALFEATURE_ON
			#pragma shader_feature _ALPHAMASKFEATURE_ON


			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 positionOld : TEXCOORD4;
				#if _ADD_PRECOMPUTED_VELOCITY
					float3 alembicMotionVector : TEXCOORD5;
				#endif
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				float4 positionCS : SV_POSITION;
				float4 positionCSNoJitter : TEXCOORD0;
				float4 previousPositionCSNoJitter : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_texcoord8 : TEXCOORD8;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainColor;
			float3 _LineGlitchOffset;
			float3 _MaskSize;
			float3 _MaskCenter;
			float3 _DissolveScale;
			float3 _RandomGlitchOffset;
			float3 _GrainScale;
			float2 _GrainValues;
			float _CullMode;
			float _Line2Frequency;
			float _Line2Speed;
			float _Line2InvertedThickness;
			float _Line2Hardness;
			float _Line2Alpha;
			float _SoftIntersection1Distance;
			float _SoftIntersection1Intensity;
			float _GrainAffect;
			float _Line1Alpha;
			float _SoftIntersection2Distance;
			float _SoftIntersection2Intensity;
			float _SoftIntersection2Affect;
			float _DissolveHide;
			float _MaskFalloff;
			float _MaskInversion;
			float _SoftIntersection1Affect;
			float _Line1Hardness;
			float _Line1Speed;
			float _AlphaMaskAffect;
			float _ZWrite;
			float _PositionDirection;
			float _LineGlitchFrequency;
			float _LineGlitchSpeed;
			float _RandomOffset;
			float _LineGlitchInvertedThickness;
			float _LineGlitchHardness;
			float _RandomGlitchTiling;
			float _RandomGlitchConstant;
			float _Line1InvertedThickness;
			float _RandomGlitchAmount;
			float _VoxelizationAffect;
			float _ColorGlitchAffect;
			float _FresnelScale;
			float _FresnelPower;
			float _NormalScale;
			float _NormalAffect;
			float _FresnelAlphaScale;
			float _FresnelAlphaPower;
			float _Line1Frequency;
			float _Voxelization;
			float _Alpha;
			#ifdef ASE_TRANSMISSION
				float _TransmissionShadow;
			#endif
			#ifdef ASE_TRANSLUCENCY
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			#ifdef SCENEPICKINGPASS
				float4 _SelectionID;
			#endif

			#ifdef SCENESELECTIONPASS
				int _ObjectId;
				int _PassValue;
			#endif

			sampler2D _LineGlitch;
			sampler2D _NormalMap;
			sampler2D _Line1;
			sampler2D _Line2;
			sampler2D _AlphaMask;
			UNITY_INSTANCING_BUFFER_START(KnifeHologramShaderUnlit)
				UNITY_DEFINE_INSTANCED_PROP(float4x4, _CustomMatrix)
				UNITY_DEFINE_INSTANCED_PROP(float4, _NormalMap_ST)
				UNITY_DEFINE_INSTANCED_PROP(float4, _AlphaMask_ST)
			UNITY_INSTANCING_BUFFER_END(KnifeHologramShaderUnlit)


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float3 mod3D289( float3 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 mod3D289( float4 x ) { return x - floor( x / 289.0 ) * 289.0; }
			float4 permute( float4 x ) { return mod3D289( ( x * 34.0 + 1.0 ) * x ); }
			float4 taylorInvSqrt( float4 r ) { return 1.79284291400159 - r * 0.85373472095314; }
			float snoise( float3 v )
			{
				const float2 C = float2( 1.0 / 6.0, 1.0 / 3.0 );
				float3 i = floor( v + dot( v, C.yyy ) );
				float3 x0 = v - i + dot( i, C.xxx );
				float3 g = step( x0.yzx, x0.xyz );
				float3 l = 1.0 - g;
				float3 i1 = min( g.xyz, l.zxy );
				float3 i2 = max( g.xyz, l.zxy );
				float3 x1 = x0 - i1 + C.xxx;
				float3 x2 = x0 - i2 + C.yyy;
				float3 x3 = x0 - 0.5;
				i = mod3D289( i);
				float4 p = permute( permute( permute( i.z + float4( 0.0, i1.z, i2.z, 1.0 ) ) + i.y + float4( 0.0, i1.y, i2.y, 1.0 ) ) + i.x + float4( 0.0, i1.x, i2.x, 1.0 ) );
				float4 j = p - 49.0 * floor( p / 49.0 );  // mod(p,7*7)
				float4 x_ = floor( j / 7.0 );
				float4 y_ = floor( j - 7.0 * x_ );  // mod(j,N)
				float4 x = ( x_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 y = ( y_ * 2.0 + 0.5 ) / 7.0 - 1.0;
				float4 h = 1.0 - abs( x ) - abs( y );
				float4 b0 = float4( x.xy, y.xy );
				float4 b1 = float4( x.zw, y.zw );
				float4 s0 = floor( b0 ) * 2.0 + 1.0;
				float4 s1 = floor( b1 ) * 2.0 + 1.0;
				float4 sh = -step( h, 0.0 );
				float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy;
				float4 a1 = b1.xzyw + s1.xzyw * sh.zzww;
				float3 g0 = float3( a0.xy, h.x );
				float3 g1 = float3( a0.zw, h.y );
				float3 g2 = float3( a1.xy, h.z );
				float3 g3 = float3( a1.zw, h.w );
				float4 norm = taylorInvSqrt( float4( dot( g0, g0 ), dot( g1, g1 ), dot( g2, g2 ), dot( g3, g3 ) ) );
				g0 *= norm.x;
				g1 *= norm.y;
				g2 *= norm.z;
				g3 *= norm.w;
				float4 m = max( 0.6 - float4( dot( x0, x0 ), dot( x1, x1 ), dot( x2, x2 ), dot( x3, x3 ) ), 0.0 );
				m = m* m;
				m = m* m;
				float4 px = float4( dot( x0, g0 ), dot( x1, g1 ), dot( x2, g2 ), dot( x3, g3 ) );
				return 42.0 * dot( m, px);
			}
			

			PackedVaryings VertexFunction( Attributes input  )
			{
				PackedVaryings output = (PackedVaryings)0;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float C_ZERO287 = 0.0;
				float3 temp_cast_0 = (C_ZERO287).xxx;
				float3 viewToObjDir94 = mul( UNITY_MATRIX_T_MV, float4( _LineGlitchOffset, 0 ) ).xyz;
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float3 ase_positionWS = TransformObjectToWorld( (input.positionOS).xyz );
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch350 = ase_positionWS.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch350 = ase_positionWS.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch350 = ase_positionWS.z;
				#else
				float staticSwitch350 = ase_positionWS.y;
				#endif
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch351 = input.positionOS.xyz.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch351 = input.positionOS.xyz.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch351 = input.positionOS.xyz.z;
				#else
				float staticSwitch351 = input.positionOS.xyz.y;
				#endif
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.positionOS.xyz , 0.0 ) ).xyz;
				float3 break379 = temp_output_377_0;
				#if defined( _POSITIONFEATURE_X )
				float staticSwitch378 = break379.x;
				#elif defined( _POSITIONFEATURE_Y )
				float staticSwitch378 = break379.y;
				#elif defined( _POSITIONFEATURE_Z )
				float staticSwitch378 = break379.z;
				#else
				float staticSwitch378 = break379.y;
				#endif
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch352 = staticSwitch350;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch352 = staticSwitch351;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch352 = staticSwitch378;
				#else
				float staticSwitch352 = staticSwitch350;
				#endif
				float vertexToFrag497 = ( staticSwitch352 * _PositionDirection );
				float pos353 = vertexToFrag497;
				float mulTime3_g167 = _TimeParameters.x * _LineGlitchSpeed;
				float randomoffset385 = _RandomOffset;
				float2 temp_cast_3 = ((pos353*_LineGlitchFrequency + ( mulTime3_g167 + randomoffset385 ))).xx;
				float clampResult42_g167 = clamp( ( ( tex2Dlod( _LineGlitch, float4( temp_cast_3, 0, 0.0) ).r - _LineGlitchInvertedThickness ) * _LineGlitchHardness ) , 0.0 , 1.0 );
				#ifdef _LINEGLITCHFEATURE_ON
				float3 staticSwitch307 = ( ( viewToObjDir94 / ase_objectScale ) * clampResult42_g167 );
				#else
				float3 staticSwitch307 = temp_cast_0;
				#endif
				float3 lineglitch491 = staticSwitch307;
				float3 temp_cast_4 = (C_ZERO287).xxx;
				float3 viewToObjDir122 = mul( UNITY_MATRIX_T_MV, float4( _RandomGlitchOffset, 0 ) ).xyz;
				float mulTime149 = _TimeParameters.x * -2.3;
				float mulTime155 = _TimeParameters.x * -2.05;
				float2 appendResult153 = (float2((pos353*_RandomGlitchTiling + ( mulTime149 + randomoffset385 )) , ( randomoffset385 + mulTime155 )));
				float simplePerlin2D186 = snoise( appendResult153 );
				simplePerlin2D186 = simplePerlin2D186*0.5 + 0.5;
				float4 matrixToPos373 = float4( float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[0][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[1][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[2][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[3][3]);
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch365 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch365 = 0.0;
				#else
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#endif
				float mulTime139 = _TimeParameters.x * -5.74;
				float mulTime157 = _TimeParameters.x * -0.83;
				float2 appendResult158 = (float2((staticSwitch365*223.0 + ( mulTime139 + randomoffset385 )) , ( randomoffset385 + mulTime157 )));
				float simplePerlin2D187 = snoise( appendResult158 );
				simplePerlin2D187 = simplePerlin2D187*0.5 + 0.5;
				float clampResult148 = clamp( (-1.0 + (( simplePerlin2D187 + _RandomGlitchConstant ) - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float temp_output_197_0 = ( (-1.0 + (simplePerlin2D186 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) * clampResult148 );
				float2 break190 = appendResult153;
				float2 appendResult195 = (float2(( 20.0 * break190.x ) , break190.y));
				float simplePerlin2D188 = snoise( appendResult195 );
				simplePerlin2D188 = simplePerlin2D188*0.5 + 0.5;
				float clampResult192 = clamp( (-1.0 + (simplePerlin2D188 - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult199 = lerp( 0.0 , clampResult192 , 2.0);
				#ifdef _RANDOMGLITCHFEATURE_ON
				float3 staticSwitch311 = ( ( viewToObjDir122 / ase_objectScale ) * ( temp_output_197_0 + ( temp_output_197_0 * lerpResult199 ) ) * _RandomGlitchAmount );
				#else
				float3 staticSwitch311 = temp_cast_4;
				#endif
				float3 randomglitch493 = staticSwitch311;
				float3 vertexoffset410 = ( lineglitch491 + randomglitch493 );
				float3 temp_output_512_0 = ( input.positionOS.xyz + vertexoffset410 );
				float3 lerpResult517 = lerp( temp_output_512_0 , ( round( ( temp_output_512_0 * _Voxelization ) ) / _Voxelization ) , _VoxelizationAffect);
				#ifdef _VOXELIZATIONFEATURE_ON
				float3 staticSwitch518 = lerpResult517;
				#else
				float3 staticSwitch518 = temp_output_512_0;
				#endif
				float3 ModifiedVertexPosition519 = staticSwitch518;
				
				output.ase_texcoord2.xyz = ase_positionWS;
				float3 ase_normalWS = TransformObjectToWorldNormal(input.ase_normal);
				output.ase_texcoord3.xyz = ase_normalWS;
				float3 ase_tangentWS = TransformObjectToWorldDir(input.ase_tangent.xyz);
				output.ase_texcoord5.xyz = ase_tangentWS;
				float ase_tangentSign = input.ase_tangent.w * ( unity_WorldTransformParams.w >= 0.0 ? 1.0 : -1.0 );
				float3 ase_bitangentWS = cross( ase_normalWS, ase_tangentWS ) * ase_tangentSign;
				output.ase_texcoord6.xyz = ase_bitangentWS;
				output.ase_texcoord2.w = vertexToFrag497;
				float3 vertexPos238 = ( input.positionOS.xyz + vertexoffset410 );
				float4 ase_positionCS238 = TransformObjectToHClip((vertexPos238).xyz);
				float4 screenPos238 = ComputeScreenPos(ase_positionCS238);
				output.ase_texcoord7 = screenPos238;
				float3 vertexPos417 = ( input.positionOS.xyz + vertexoffset410 );
				
				output.ase_texcoord4.xy = input.ase_texcoord.xy;
				output.ase_texcoord8 = input.positionOS;
				output.ase_color = input.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord3.w = 0;
				output.ase_texcoord4.zw = 0;
				output.ase_texcoord5.w = 0;
				output.ase_texcoord6.w = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ModifiedVertexPosition519;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				VertexPositionInputs vertexInput = GetVertexPositionInputs( input.positionOS.xyz );

				// Jittered. Match the frame.
				output.positionCS = vertexInput.positionCS;
				output.positionCSNoJitter = mul( _NonJitteredViewProjMatrix, mul( UNITY_MATRIX_M, input.positionOS ) );

				float4 prevPos = ( unity_MotionVectorsParams.x == 1 ) ? float4( input.positionOld, 1 ) : input.positionOS;

				#if _ADD_PRECOMPUTED_VELOCITY
					prevPos = prevPos - float4(input.alembicMotionVector, 0);
				#endif

				output.previousPositionCSNoJitter = mul( _PrevViewProjMatrix, mul( UNITY_PREV_MATRIX_M, prevPos ) );

				return output;
			}

			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}

			half4 frag(	PackedVaryings input  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( input );

				float3 objToWorld161 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				float C_ZERO287 = 0.0;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch360 = C_ZERO287;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float staticSwitch360 = 0.0;
				#else
				float staticSwitch360 = ( objToWorld161.x + objToWorld161.y + objToWorld161.z );
				#endif
				float randomoffset385 = _RandomOffset;
				float mulTime165 = _TimeParameters.x * -15.0;
				float mulTime167 = _TimeParameters.x * -0.5;
				float2 appendResult168 = (float2((staticSwitch360*223.0 + ( randomoffset385 + mulTime165 )) , ( randomoffset385 + mulTime167 )));
				float simplePerlin2D176 = snoise( appendResult168 );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float clampResult175 = clamp( (-0.61 + (simplePerlin2D176 - 0.0) * (2.0 - -0.61) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult181 = lerp( 1.0 , clampResult175 , _ColorGlitchAffect);
				#ifdef _COLORGLITCHFEATURE_ON
				float staticSwitch279 = lerpResult181;
				#else
				float staticSwitch279 = 1.0;
				#endif
				float4 maincolor232 = _MainColor;
				float C_ONE288 = 1.0;
				float3 ase_positionWS = input.ase_texcoord2.xyz;
				float3 ase_viewVectorWS = ( _WorldSpaceCameraPos.xyz - ase_positionWS );
				float3 ase_viewDirWS = normalize( ase_viewVectorWS );
				float3 ase_normalWS = input.ase_texcoord3.xyz;
				float fresnelNdotV2 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode2 = ( 0.0 + _FresnelScale * pow( max( 1.0 - fresnelNdotV2 , 0.0001 ), _FresnelPower ) );
				float4 _NormalMap_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_NormalMap_ST);
				float2 uv_NormalMap = input.ase_texcoord4.xy * _NormalMap_ST_Instance.xy + _NormalMap_ST_Instance.zw;
				float3 unpack95 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalScale );
				unpack95.z = lerp( 1, unpack95.z, saturate(_NormalScale) );
				float3 ase_tangentWS = input.ase_texcoord5.xyz;
				float3 ase_bitangentWS = input.ase_texcoord6.xyz;
				float3 tanToWorld0 = float3( ase_tangentWS.x, ase_bitangentWS.x, ase_normalWS.x );
				float3 tanToWorld1 = float3( ase_tangentWS.y, ase_bitangentWS.y, ase_normalWS.y );
				float3 tanToWorld2 = float3( ase_tangentWS.z, ase_bitangentWS.z, ase_normalWS.z );
				float3 ase_viewVectorTS =  tanToWorld0 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).x + tanToWorld1 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).y  + tanToWorld2 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).z;
				float3 ase_viewDirTS = normalize( ase_viewVectorTS );
				float dotResult101 = dot( unpack95 , ase_viewDirTS );
				float lerpResult106 = lerp( 1.0 , (0.0 + (dotResult101 - -1.0) * (1.0 - 0.0) / (1.0 - -1.0)) , _NormalAffect);
				#ifdef _NORMALMAPFEATURE_ON
				float staticSwitch341 = ( 1.0 - lerpResult106 );
				#else
				float staticSwitch341 = C_ZERO287;
				#endif
				float NormalAffect116 = staticSwitch341;
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch303 = ( fresnelNode2 + NormalAffect116 );
				#else
				float staticSwitch303 = C_ONE288;
				#endif
				float fresnelNdotV53 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode53 = ( 0.0 + _FresnelAlphaScale * pow( max( 1.0 - fresnelNdotV53 , 0.0001 ), _FresnelAlphaPower ) );
				#ifdef _FRESNELFEATURE_ON
				float staticSwitch305 = fresnelNode53;
				#else
				float staticSwitch305 = C_ZERO287;
				#endif
				float clampResult57 = clamp( ( staticSwitch305 + NormalAffect116 ) , 0.0 , 1.0 );
				float4 fresnelcolor436 = ( staticSwitch303 * maincolor232 * clampResult57 );
				float4 temp_cast_0 = (C_ZERO287).xxxx;
				float vertexToFrag497 = input.ase_texcoord2.w;
				float pos353 = vertexToFrag497;
				float mulTime3_g169 = _TimeParameters.x * _Line1Speed;
				float2 temp_cast_1 = ((pos353*_Line1Frequency + ( mulTime3_g169 + randomoffset385 ))).xx;
				float clampResult42_g169 = clamp( ( ( tex2D( _Line1, temp_cast_1 ).r - _Line1InvertedThickness ) * _Line1Hardness ) , 0.0 , 1.0 );
				float temp_output_536_0 = clampResult42_g169;
				float3 temp_output_293_0 = (( maincolor232 * temp_output_536_0 )).rgb;
				float temp_output_61_0 = ( temp_output_536_0 * _Line1Alpha );
				float4 appendResult292 = (float4(temp_output_293_0 , temp_output_61_0));
				#ifdef _LINE1FEATURE_ON
				float4 staticSwitch284 = appendResult292;
				#else
				float4 staticSwitch284 = temp_cast_0;
				#endif
				float mulTime3_g168 = _TimeParameters.x * _Line2Speed;
				float2 temp_cast_2 = ((pos353*_Line2Frequency + ( mulTime3_g168 + randomoffset385 ))).xx;
				float clampResult42_g168 = clamp( ( ( tex2D( _Line2, temp_cast_2 ).r - _Line2InvertedThickness ) * _Line2Hardness ) , 0.0 , 1.0 );
				float temp_output_535_0 = clampResult42_g168;
				float3 temp_output_294_0 = (( maincolor232 * temp_output_535_0 )).rgb;
				float temp_output_69_0 = ( temp_output_535_0 * _Line2Alpha );
				float4 appendResult295 = (float4(temp_output_294_0 , temp_output_69_0));
				#ifdef _LINE2FEATURE_ON
				float4 staticSwitch285 = appendResult295;
				#else
				float4 staticSwitch285 = staticSwitch284;
				#endif
				float4 appendResult328 = (float4(0.0 , 0.0 , 0.0 , temp_output_61_0));
				float4 appendResult296 = (float4(( temp_output_293_0 * temp_output_294_0 ) , ( temp_output_61_0 * temp_output_69_0 )));
				#ifdef _LINEBOTHFEATURE_ON
				float4 staticSwitch286 = ( appendResult328 + appendResult296 );
				#else
				float4 staticSwitch286 = staticSwitch285;
				#endif
				float4 line_color298 = staticSwitch286;
				float4 temp_cast_4 = (C_ZERO287).xxxx;
				float4 screenPos238 = input.ase_texcoord7;
				float4 ase_positionSSNorm = screenPos238 / screenPos238.w;
				ase_positionSSNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_positionSSNorm.z : ase_positionSSNorm.z * 0.5 + 0.5;
				float screenDepth238 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth238 = saturate( abs( ( screenDepth238 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection1Distance ) ) );
				float saferPower344 = abs( distanceDepth238 );
				float lerpResult467 = lerp( 1.0 , pow( saferPower344 , _SoftIntersection1Intensity ) , _SoftIntersection1Affect);
				float4 appendResult321 = (float4(0.0 , 0.0 , 0.0 , lerpResult467));
				float temp_output_242_0 = ( 1.0 - distanceDepth238 );
				float4 appendResult314 = (float4((( maincolor232 * temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )).rgb , ( temp_output_242_0 * _SoftIntersection1Intensity * _SoftIntersection1Affect )));
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float4 staticSwitch315 = temp_cast_4;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float4 staticSwitch315 = appendResult321;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float4 staticSwitch315 = appendResult314;
				#else
				float4 staticSwitch315 = temp_cast_4;
				#endif
				float4 intersection1245 = staticSwitch315;
				float mulTime273 = _TimeParameters.x * 100.0;
				float simplePerlin3D264 = snoise( (ase_positionWS*_GrainScale + mulTime273) );
				simplePerlin3D264 = simplePerlin3D264*0.5 + 0.5;
				float lerpResult278 = lerp( _GrainValues.x , _GrainValues.y , simplePerlin3D264);
				float lerpResult269 = lerp( 0.0 , lerpResult278 , _GrainAffect);
				#ifdef _GRAINFEATURE_ON
				float staticSwitch282 = lerpResult269;
				#else
				float staticSwitch282 = 0.0;
				#endif
				float grain268 = staticSwitch282;
				float4 temp_cast_6 = (C_ZERO287).xxxx;
				float screenDepth417 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_positionSSNorm.xy ),_ZBufferParams);
				float distanceDepth417 = saturate( abs( ( screenDepth417 - LinearEyeDepth( ase_positionSSNorm.z,_ZBufferParams ) ) / ( _SoftIntersection2Distance ) ) );
				float saferPower422 = abs( distanceDepth417 );
				float lerpResult471 = lerp( 1.0 , pow( saferPower422 , _SoftIntersection2Intensity ) , _SoftIntersection2Affect);
				float4 appendResult424 = (float4(0.0 , 0.0 , 0.0 , lerpResult471));
				float temp_output_418_0 = ( 1.0 - distanceDepth417 );
				float4 appendResult425 = (float4((( maincolor232 * temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )).rgb , ( temp_output_418_0 * _SoftIntersection2Intensity * _SoftIntersection2Affect )));
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float4 staticSwitch429 = temp_cast_6;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float4 staticSwitch429 = appendResult424;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float4 staticSwitch429 = appendResult425;
				#else
				float4 staticSwitch429 = temp_cast_6;
				#endif
				float4 intersection2430 = staticSwitch429;
				float fresnelalpha434 = clampResult57;
				float intersectionalpha1261 = (staticSwitch315).w;
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch330 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch330 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch330 = intersectionalpha1261;
				#else
				float staticSwitch330 = intersectionalpha1261;
				#endif
				float intersectionalpha2431 = (staticSwitch429).w;
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch447 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch447 = C_ZERO287;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch447 = intersectionalpha2431;
				#else
				float staticSwitch447 = intersectionalpha2431;
				#endif
				#if defined( _SOFTINTERSECTION1FEATURE_OFF )
				float staticSwitch334 = C_ONE288;
				#elif defined( _SOFTINTERSECTION1FEATURE_ALPHA )
				float staticSwitch334 = intersectionalpha1261;
				#elif defined( _SOFTINTERSECTION1FEATURE_COLOR )
				float staticSwitch334 = C_ONE288;
				#else
				float staticSwitch334 = C_ONE288;
				#endif
				#if defined( _SOFTINTERSECTION2FEATURE_OFF )
				float staticSwitch448 = C_ONE288;
				#elif defined( _SOFTINTERSECTION2FEATURE_ALPHA )
				float staticSwitch448 = intersectionalpha2431;
				#elif defined( _SOFTINTERSECTION2FEATURE_COLOR )
				float staticSwitch448 = C_ONE288;
				#else
				float staticSwitch448 = C_ONE288;
				#endif
				float clampResult62 = clamp( ( ( (maincolor232).a + fresnelalpha434 + (line_color298).w + staticSwitch330 + staticSwitch447 ) * staticSwitch334 * staticSwitch448 ) , 0.0 , 1.0 );
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_CustomMatrix);
				float3 temp_output_377_0 = mul( _CustomMatrix_Instance, float4( input.ase_texcoord8.xyz , 0.0 ) ).xyz;
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float3 staticSwitch375 = ase_positionWS;
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float3 staticSwitch375 = input.ase_texcoord8.xyz;
				#elif defined( _POSITIONSPACEFEATURE_CUSTOM )
				float3 staticSwitch375 = temp_output_377_0;
				#else
				float3 staticSwitch375 = ase_positionWS;
				#endif
				float3 vPos357 = staticSwitch375;
				float simplePerlin3D212 = snoise( ( vPos357 * _DissolveScale ) );
				simplePerlin3D212 = simplePerlin3D212*0.5 + 0.5;
				float clampResult218 = clamp( ( simplePerlin3D212 - _DissolveHide ) , 0.0 , 1.0 );
				#ifdef _DISSOLVEFEATURE_ON
				float staticSwitch337 = clampResult218;
				#else
				float staticSwitch337 = C_ONE288;
				#endif
				float dissolve432 = staticSwitch337;
				float3 objToWorld257 = mul( GetObjectToWorldMatrix(), float4( float3( 0,0,0 ), 1 ) ).xyz;
				#ifdef _MASKLOCALFEATURE_ON
				float3 staticSwitch499 = ( _MaskCenter + objToWorld257 );
				#else
				float3 staticSwitch499 = _MaskCenter;
				#endif
				float clampResult256 = clamp( ( distance( max( ( abs( ( ase_positionWS - staticSwitch499 ) ) - ( _MaskSize * float3( 0.5,0.5,0.5 ) ) ) , float3( 0,0,0 ) ) , float3( 0,0,0 ) ) / _MaskFalloff ) , 0.0 , 1.0 );
				float lerpResult501 = lerp( clampResult256 , ( 1.0 - clampResult256 ) , _MaskInversion);
				#ifdef _MASKFEATURE_ON
				float staticSwitch339 = lerpResult501;
				#else
				float staticSwitch339 = C_ONE288;
				#endif
				float mask254 = staticSwitch339;
				float4 _AlphaMask_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(KnifeHologramShaderUnlit,_AlphaMask_ST);
				float2 uv_AlphaMask = input.ase_texcoord4.xy * _AlphaMask_ST_Instance.xy + _AlphaMask_ST_Instance.zw;
				float lerpResult400 = lerp( C_ONE288 , tex2D( _AlphaMask, uv_AlphaMask ).r , _AlphaMaskAffect);
				#ifdef _ALPHAMASKFEATURE_ON
				float staticSwitch404 = lerpResult400;
				#else
				float staticSwitch404 = C_ONE288;
				#endif
				float alphamask402 = staticSwitch404;
				float4 appendResult325 = (float4((( staticSwitch279 * ( maincolor232 + fresnelcolor436 + float4( (line_color298).xyz , 0.0 ) + intersection1245 + grain268 + intersection2430 ) )).rgb , ( clampResult62 * dissolve432 * mask254 * alphamask402 * _Alpha )));
				float4 temp_output_504_0 = ( appendResult325 * input.ase_color );
				

				float Alpha = (temp_output_504_0).w;
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( input.positionCS );
				#endif

				return float4( CalcNdcMotionVectorFromCsPositions( input.positionCSNoJitter, input.previousPositionCSNoJitter ), 0, 0 );
			}
			ENDHLSL
		}
		
	}
	
	CustomEditor "Knife.HologramEffect.HologramShaderEditor"
	FallBack "Hidden/Shader Graph/FallbackError"
	
	Fallback "Hidden/InternalErrorShader"
}
/*ASEBEGIN
Version=19800
Node;AmplifyShaderEditor.Matrix4X4Node;374;-6291.629,1818.142;Inherit;False;InstancedProperty;_CustomMatrix;Custom Matrix;61;0;Create;True;0;0;0;False;0;False;1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1;0;1;FLOAT4x4;0
Node;AmplifyShaderEditor.PosVertexDataNode;381;-6222.629,2025.142;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;377;-5830.629,1755.142;Inherit;False;2;2;0;FLOAT4x4;0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PosVertexDataNode;349;-5800.494,1460.739;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WorldPosInputsNode;348;-5814.094,1236.639;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.BreakToComponentsNode;379;-5666.629,1633.142;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.StaticSwitch;351;-5514.092,1413.739;Inherit;False;Property;_PositionFeature;Position Feature;52;0;Create;True;0;0;0;False;0;False;0;1;1;True;;KeywordEnum;3;X;Y;Z;Reference;-1;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;378;-5318.629,1635.142;Inherit;False;Property;_PositionFeature;Position Feature;58;0;Create;True;0;0;0;False;0;False;0;1;1;True;;KeywordEnum;3;X;Y;Z;Reference;350;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;350;-5485.891,1289.339;Inherit;False;Property;_PositionFeature;Position Feature;58;0;Create;True;0;0;0;False;0;False;0;1;1;True;;KeywordEnum;3;X;Y;Z;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;310;-4914.948,2848.478;Inherit;False;5020.701;2224.457;Random Glitch;28;493;311;123;312;196;128;122;201;121;197;199;198;192;148;124;191;147;186;129;126;202;127;208;210;211;209;506;505;;1,1,1,1;0;0
Node;AmplifyShaderEditor.StaticSwitch;352;-4877.792,1320.339;Inherit;False;Property;_PositionSpaceFeature;Position Space Feature;59;0;Create;True;0;0;0;False;0;False;0;0;0;True;;KeywordEnum;3;World;Local;Custom;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;368;-4589.016,1401.59;Inherit;False;Property;_PositionDirection;Position Direction;60;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;209;-4864.948,3810.819;Inherit;False;1004.303;765.5337;Main Glitch Cycle;12;150;151;131;149;154;155;152;153;364;393;392;394;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;384;-4085.654,750.971;Inherit;False;Property;_RandomOffset;Random Offset;62;0;Create;True;0;0;0;False;0;False;0;73887;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;367;-4324.016,1335.59;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.VertexToFragmentNode;497;-4100.132,1423.022;Inherit;False;False;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;385;-3907.411,764.0204;Inherit;False;randomoffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;150;-4856.948,4345.908;Inherit;False;Constant;_Glitch1Speed;Glitch 1 Speed;41;0;Create;True;0;0;0;False;0;False;-2.3;-2.3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;353;-3814.792,1379.339;Inherit;False;pos;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;154;-4656.182,4461.353;Inherit;False;Constant;_Glitch1SpeedX;Glitch 1 Speed X;43;0;Create;True;0;0;0;False;0;False;-2.05;-2.05;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;149;-4668.948,4246.908;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;393;-4666.475,4335.419;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;211;-3538.16,4021.232;Inherit;False;1719.862;594;Random Glitch;19;365;187;225;158;157;142;156;141;137;139;140;138;226;366;373;387;389;391;388;;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;364;-4772.997,4006.281;Inherit;False;353;pos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;289;-4087.642,1043.74;Inherit;False;Constant;_ConstantZero;Constant Zero;47;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;394;-4461.103,4174.139;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;151;-4777.948,4102.908;Inherit;False;Property;_RandomGlitchTiling;Random Glitch Tiling;44;0;Create;True;0;0;0;False;0;False;2.83;2.83;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;155;-4403.509,4363.612;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PosFromTransformMatrix;373;-3524.308,4214.366;Inherit;False;1;0;FLOAT4x4;1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ScaleAndOffsetNode;152;-4430.125,3953.91;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;138;-3366.16,4498.232;Inherit;False;Constant;_GlitchPeriodicSpeed;Glitch Periodic Speed;44;0;Create;True;0;0;0;False;0;False;-5.74;-5.74;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;287;-3856,1024;Inherit;False;C_ZERO;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;392;-4273.475,4229.419;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;141;-3242.16,4093.232;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;366;-3240.307,4218.652;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;139;-3110.16,4420.232;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;156;-2881.471,4532.902;Inherit;False;Constant;_GlitchPeriodicSpeedX;Glitch Periodic Speed X;45;0;Create;True;0;0;0;False;0;False;-0.83;-0.83;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;387;-3127.891,4498.345;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;153;-4027.645,4059.354;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;210;-2893.955,4715.682;Inherit;False;764.9996;344.855;Small Glitch lines;5;193;190;194;195;188;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;137;-3212.16,4334.232;Inherit;False;Constant;_GlitchPeriodicTiling;Glitch Periodic Tiling;39;0;Create;True;0;0;0;False;0;False;223;223;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;391;-2748.358,4324.07;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;157;-2658.471,4423.902;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;208;-3461.354,4815.481;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.StaticSwitch;365;-3097.307,4068.652;Inherit;False;Property;_Keyword2;Keyword 2;59;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;352;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;388;-2913.277,4404.269;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;142;-2779.339,4100.235;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;190;-2843.954,4927.537;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.RangedFloatNode;193;-2838.523,4828.179;Inherit;False;Constant;_Float2;Float 2;34;0;Create;True;0;0;0;False;0;False;20;20.35;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;389;-2512.358,4319.07;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;194;-2648.448,4765.682;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;158;-2500.971,4132.902;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;195;-2511.954,4805.537;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;187;-2325.298,4163.958;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;225;-2436.07,4416.123;Inherit;False;Property;_RandomGlitchConstant;Random Glitch Constant;37;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;202;-2318.141,3667.232;Inherit;False;Constant;_GlitchRemapMinNew;Glitch Remap Min New;24;0;Create;True;0;0;0;False;0;False;-1;-1;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;226;-1999.07,4277.123;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;126;-2320.267,3446.609;Inherit;False;Constant;_GlitchRemapMinOld;Glitch Remap Min Old;24;0;Create;True;0;0;0;False;0;False;0;0;-2;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;188;-2360.706,4809.289;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;129;-2332.363,3765.809;Inherit;False;Constant;_GlitchRemapMaxNew;Glitch Remap Max New;26;0;Create;True;0;0;0;False;0;False;1;1;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;127;-2315.007,3551.194;Inherit;False;Constant;_GlitchRemapMaxOld;Glitch Remap Max Old;25;0;Create;True;0;0;0;False;0;False;1;1;-2;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;147;-1533.459,3705.366;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;186;-1816.782,3105.236;Inherit;True;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;191;-1542.325,3940.872;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;85;-2089.168,1936.791;Inherit;False;1829.211;723.5701;Glitch Line;15;81;94;87;90;86;89;84;88;307;308;363;479;491;507;508;;1,1,1,1;0;0
Node;AmplifyShaderEditor.ClampOpNode;148;-1268.509,3758.792;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;198;-1184.161,4263.347;Inherit;False;Constant;_Float3;Float 3;35;0;Create;True;0;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;192;-1302.91,4039.199;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;124;-1536.032,3511.749;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;199;-1061.773,3992.978;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;121;-1814.001,2898.478;Inherit;False;Property;_RandomGlitchOffset;Random Glitch Offset;15;0;Create;True;0;0;0;False;0;False;-0.5,0,0;-0.5,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;197;-1109.023,3544.452;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;84;-1704.07,1985.222;Inherit;False;Property;_LineGlitchOffset;Line Glitch Offset;14;0;Create;True;0;0;0;False;0;False;0.03,0,0;0.03,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.ObjectScaleNode;505;-1491.978,3147.317;Inherit;False;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.ObjectScaleNode;507;-1488.19,2206.576;Inherit;False;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;479;-1964.807,2162.083;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TransformDirectionNode;94;-1500.492,2031.106;Inherit;False;View;Object;False;Fast;False;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.TexturePropertyNode;89;-1994.981,1975.198;Inherit;True;Property;_LineGlitch;Line Glitch;13;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;88eb97e78f86c604bb00864c0dbeffc1;a6ddbfbf28b5acb44921ca832c5c7f18;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;87;-1985.12,2442.207;Inherit;False;Property;_LineGlitchFrequency;Line Glitch Frequency;19;0;Create;True;0;0;0;False;0;False;0.2;0.2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;90;-1987.701,2358.637;Inherit;False;Property;_LineGlitchHardness;Line Glitch Hardness;20;0;Create;True;0;0;0;False;0;False;5;5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;363;-1866.639,2236.446;Inherit;False;353;pos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;201;-973.1417,3683.593;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;86;-1985.9,2516.262;Inherit;False;Property;_LineGlitchInvertedThickness;Line Glitch Inverted Thickness;21;0;Create;True;0;0;0;False;0;False;0.825;0.848;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TransformDirectionNode;122;-1541.001,2981.677;Inherit;False;View;Object;False;Fast;False;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;88;-2067.168,2296.9;Inherit;False;Property;_LineGlitchSpeed;Line Glitch Speed;18;0;Create;True;0;0;0;False;0;False;-0.26;-0.26;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;128;-1175.329,3161.073;Inherit;False;Property;_RandomGlitchAmount;Random Glitch Amount;16;0;Create;True;0;0;0;False;0;False;0.089;0.122;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;196;-888.1868,3461.786;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;506;-1231.031,2993.553;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;508;-1248.843,2089.367;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;537;-1532.734,2384.947;Inherit;False;Hologram Line;-1;;167;a6b4840f4c8a45041b49734edbb63562;0;7;37;SAMPLER2D;0;False;44;FLOAT;0;False;43;FLOAT;0;False;13;FLOAT;1;False;14;FLOAT;1;False;15;FLOAT;2;False;16;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;312;-665.0436,2941.625;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;123;-742.2883,3021.861;Inherit;False;3;3;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;81;-1091.057,2172.398;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;308;-1093.97,2020.648;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;311;-476.2436,3031.325;Inherit;False;Property;_RandomGlitchFeature;Random Glitch Feature;49;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;307;-898.1913,2090.678;Inherit;False;Property;_LineGlitchFeature;Line Glitch Feature;48;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;491;-555.8857,2103.779;Inherit;False;lineglitch;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;493;-133.1834,3062.198;Inherit;False;randomglitch;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;492;163.2169,1382.597;Inherit;False;491;lineglitch;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;494;182.7164,1569.798;Inherit;False;493;randomglitch;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;159;608.1558,1211.944;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;108;-4740.043,1979.851;Inherit;False;2459.338;604.0734;Normal Map;11;116;113;106;104;114;101;95;102;100;341;342;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;322;-6309.657,5392.202;Inherit;False;2289.167;729.9302;Soft Intersection;20;239;238;247;248;242;244;313;314;315;261;316;321;245;344;409;412;411;461;465;467;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;413;-6233.782,6283.463;Inherit;False;2399.167;724.9302;Soft Intersection;21;430;431;463;428;426;429;424;425;423;421;460;422;420;418;419;417;416;427;414;415;471;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;410;752.5458,1228.923;Inherit;False;vertexoffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;302;-4750.071,-1569.722;Inherit;False;2924.695;1595.913;Lines;11;63;67;228;229;284;296;285;286;298;327;328;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;100;-4690.043,2184.166;Inherit;False;Property;_NormalScale;NormalScale;27;0;Create;True;0;0;0;False;0;False;0;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;412;-6290.911,5878.647;Inherit;False;410;vertexoffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PosVertexDataNode;409;-6283.292,5682.31;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;414;-6215.036,6769.908;Inherit;False;410;vertexoffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PosVertexDataNode;415;-6207.417,6573.571;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;63;-4700.071,-1519.722;Inherit;False;1477.345;667.2269;Line 1;14;35;61;234;60;15;24;37;13;10;291;292;293;355;481;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;67;-4609.714,-632.8788;Inherit;False;1520.657;650.5685;Line 2;13;70;69;233;75;74;72;71;73;77;294;295;354;480;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;239;-6259.657,6007.132;Inherit;False;Property;_SoftIntersection1Distance;Soft Intersection 1 Distance;29;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;416;-5996.036,6723.908;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ColorNode;231;-4202.154,871.265;Inherit;False;Property;_MainColor;Main Color;0;1;[HDR];Create;True;0;0;0;False;0;False;0.620945,1.420074,3.953349,0.05098039;0.620945,1.420074,3.953349,0.05098039;True;True;0;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.SimpleAddOpNode;411;-6071.911,5832.647;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;427;-6183.782,6898.393;Inherit;False;Property;_SoftIntersection2Distance;Soft Intersection 2 Distance;28;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;95;-4485.904,2029.851;Inherit;True;Property;_NormalMap;NormalMap;26;0;Create;True;0;0;0;False;0;False;-1;None;d5ecf639375fb7641b0c39b0025bbf1f;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;6;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;102;-4336.033,2281.433;Inherit;False;Tangent;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.DotProductOpNode;101;-4006.033,2046.433;Inherit;True;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DepthFade;417;-5862.251,6787.997;Inherit;False;True;True;True;2;1;FLOAT3;0,0,0;False;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;71;-4581.714,-312.7708;Inherit;False;Property;_Line2Speed;Line 2 Speed;8;0;Create;True;0;0;0;False;0;False;-1;-1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;480;-4537.989,-400.0439;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DepthFade;238;-5938.126,5896.736;Inherit;False;True;True;True;2;1;FLOAT3;0,0,0;False;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;77;-4515.528,-594.4716;Inherit;True;Property;_Line2;Line 2;7;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;88eb97e78f86c604bb00864c0dbeffc1;a6ddbfbf28b5acb44921ca832c5c7f18;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;73;-4423.667,-162.8633;Inherit;False;Property;_Line2Frequency;Line 2 Frequency;9;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;13;-4514.025,-1049.706;Inherit;False;Property;_Line1Frequency;Line 1 Frequency;3;0;Create;True;0;0;0;False;0;False;100;101;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;74;-4422.446,-88.8086;Inherit;False;Property;_Line2InvertedThickness;Line 2 Inverted Thickness;11;0;Create;True;0;0;0;False;0;False;0.255;0.255;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;10;-4666.071,-1174.613;Inherit;False;Property;_Line1Speed;Line 1 Speed;2;0;Create;True;0;0;0;False;0;False;-3.57;-3.57;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;15;-4596.506,-1102.376;Inherit;False;Property;_Line1Hardness;Line 1 Hardness;4;0;Create;True;0;0;0;False;0;False;1.45;1.45;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;37;-4622.271,-1470.3;Inherit;True;Property;_Line1;Line 1;1;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;88eb97e78f86c604bb00864c0dbeffc1;ef301d4822ac111469bda406e1b4bc7c;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;72;-4502.148,-241.5338;Inherit;False;Property;_Line2Hardness;Line 2 Hardness;10;0;Create;True;0;0;0;False;0;False;2;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;24;-4512.805,-975.6513;Inherit;False;Property;_Line1InvertedThickness;Line 1 Inverted Thickness;5;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;354;-4425.068,-342.5331;Inherit;False;353;pos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;481;-4634.724,-1269.041;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;232;-3856,944;Inherit;False;maincolor;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;355;-4583.923,-1211.86;Inherit;False;353;pos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;234;-4287.936,-1356.024;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;463;-5308.75,6923.549;Inherit;False;Property;_SoftIntersection2Affect;Soft Intersection 2 Affect;33;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;242;-5640.949,5888.244;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;419;-5748.796,6443.817;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;248;-5776.702,6009.678;Inherit;False;Property;_SoftIntersection1Intensity;Soft Intersection 1 Intensity;34;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;104;-3901.555,2327.444;Inherit;False;Property;_NormalAffect;NormalAffect;30;0;Create;True;0;0;0;False;0;False;0;0.589;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;233;-4114.577,-478.7213;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;461;-5433.161,6039.013;Inherit;False;Property;_SoftIntersection1Affect;Soft Intersection 1 Affect;35;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;114;-3758.973,2022.176;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;-1;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;418;-5579.074,6789.505;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;247;-5824.671,5552.556;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;428;-5630.155,6897.453;Inherit;False;Property;_SoftIntersection2Intensity;Soft Intersection 2 Intensity;32;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;535;-4126.391,-374.1448;Inherit;False;Hologram Line;-1;;168;a6b4840f4c8a45041b49734edbb63562;0;7;37;SAMPLER2D;0;False;44;FLOAT;0;False;43;FLOAT;0;False;13;FLOAT;1;False;14;FLOAT;1;False;15;FLOAT;2;False;16;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;536;-4216.748,-1260.988;Inherit;False;Hologram Line;-1;;169;a6b4840f4c8a45041b49734edbb63562;0;7;37;SAMPLER2D;0;False;44;FLOAT;0;False;43;FLOAT;0;False;13;FLOAT;1;False;14;FLOAT;1;False;15;FLOAT;2;False;16;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;182;-2457.858,-3024.44;Inherit;False;2528.018;1042.583;Color Glitch;27;280;279;181;184;183;175;174;180;179;178;176;177;168;166;167;162;165;164;163;160;161;360;361;383;386;395;396;;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;244;-5424.438,5742.134;Inherit;False;4;4;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;106;-3560.555,2104.634;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;344;-5351.301,5586.396;Inherit;False;True;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;70;-3715.6,-497.0607;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;60;-4032.815,-998.2064;Inherit;False;Property;_Line1Alpha;Line 1 Alpha;6;0;Create;True;0;0;0;False;0;False;0.15;0.15;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;420;-5349.563,6595.395;Inherit;False;4;4;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;75;-3942.457,-111.3637;Inherit;False;Property;_Line2Alpha;Line 2 Alpha;12;0;Create;True;0;0;0;False;0;False;0.1;0.1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;422;-5210.426,6492.657;Inherit;False;True;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;35;-3813.559,-1309.803;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.CommentaryNode;323;-2176.098,5795.262;Inherit;False;1985.258;718.9717;Mask;15;254;256;249;250;253;258;252;251;257;339;340;499;500;501;502;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;281;-4547.996,-2418.382;Inherit;False;1774.707;755.3683;Grain;11;267;265;272;273;264;277;270;278;269;268;282;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;65;-3222.793,933.2142;Inherit;False;1557.666;463.3334;Fresnel Opacity;9;57;115;53;117;54;55;305;306;434;;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;342;-3281.588,2045.722;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;294;-3540.346,-455.4041;Inherit;False;True;True;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;467;-5176.697,5610.676;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;69;-3758.294,-226.0875;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;380;-5154.629,1898.142;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TransformPositionNode;161;-2412.858,-2459.758;Inherit;False;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ComponentMaskNode;293;-3662.859,-1245.96;Inherit;False;True;True;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.OneMinusNode;113;-3329.665,2126.067;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;421;-5187.16,6635.463;Inherit;False;True;True;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;313;-5263.035,5744.202;Inherit;False;True;True;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;460;-5194.136,6763.219;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;469;-5319.677,5888.282;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;160;-2427.625,-2086.218;Inherit;False;Constant;_ColorPeriodicSpeed;Color Periodic Speed;46;0;Create;True;0;0;0;False;0;False;-15;-15;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;471;-5070.44,6499.274;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;61;-3822.815,-1069.206;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;64;-3001.248,270.5131;Inherit;False;1689.588;457.9513;Fresnel;10;6;237;303;304;112;2;118;4;3;436;;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;361;-2406.056,-2558.228;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldPosInputsNode;265;-4486.409,-2368.382;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;423;-5178.159,6333.463;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TransformPositionNode;257;-2114.098,6266.84;Inherit;False;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;386;-2266.953,-2191.184;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;316;-5254.034,5442.202;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;273;-4335.223,-2095.148;Inherit;False;1;0;FLOAT;100;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;375;-4861.629,1488.142;Inherit;False;Property;_PositionSpaceFeature;Position Space Feature;59;0;Create;True;0;0;0;False;0;False;0;0;0;True;;KeywordEnum;2;World;Local;Reference;352;False;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;228;-2851.178,-722.2308;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;162;-2154.858,-2447.758;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;314;-5033.034,5786.202;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;54;-3174.455,1118.672;Inherit;False;Property;_FresnelAlphaScale;Fresnel Alpha Scale;24;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;55;-3166.455,1224.672;Inherit;False;Property;_FresnelAlphaPower;Fresnel Alpha Power;25;0;Create;True;0;0;0;False;0;False;2;3.65;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;165;-2170.858,-2114.758;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;267;-4497.996,-2222.668;Inherit;False;Property;_GrainScale;Grain Scale;39;0;Create;True;0;0;0;False;0;False;50,50,50;100,100,100;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;291;-3415.284,-1294.157;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;251;-2090.248,5993.262;Inherit;False;Property;_MaskCenter;Mask Center;31;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.DynamicAppendNode;424;-4927.159,6425.463;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;321;-5003.034,5534.202;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.StaticSwitch;341;-3088.588,2109.722;Inherit;False;Property;_NormalMapFeature;Normal Map Feature;57;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;425;-4931.159,6671.463;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;229;-2857.864,-602.0391;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;164;-2000.17,-2074.089;Inherit;False;Constant;_ColorPeriodicSpeedX;Color Periodic Speed X;45;0;Create;True;0;0;0;False;0;False;-0.5;-0.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;292;-3489.958,-1100.36;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.CommentaryNode;227;-1961.202,1464.31;Inherit;False;1755;439.4824;Dissolve Hide;10;432;337;338;218;215;214;212;221;362;220;;1,1,1,1;0;0
Node;AmplifyShaderEditor.FresnelNode;53;-2915.288,1064.339;Inherit;False;Standard;WorldNormal;ViewDir;False;True;5;0;FLOAT3;0,0,1;False;4;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;5;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;360;-2221.056,-2614.228;Inherit;False;Property;_Keyword1;Keyword 1;59;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;352;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;315;-4816.035,5658.202;Inherit;False;Property;_SoftIntersection1Feature;Soft Intersection 1 Feature;51;0;Create;True;0;0;0;False;0;False;0;0;0;True;;KeywordEnum;3;Off;Alpha;Color;Create;False;True;All;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;116;-2773.283,2151.513;Inherit;False;NormalAffect;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;258;-1831.098,6097.84;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;4;-2943.248,586.5132;Inherit;False;Property;_FresnelPower;Fresnel Power;23;0;Create;True;0;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;295;-3304.739,-406.825;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;163;-2151.858,-2283.758;Inherit;False;Constant;_ColorPeriodcTiling;Color Periodc Tiling;38;0;Create;True;0;0;0;False;0;False;223;223;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;357;-4052.389,1579.802;Inherit;False;vPos;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;429;-4716.16,6550.463;Inherit;False;Property;_SoftIntersection2Feature;Soft Intersection 2 Feature;50;0;Create;True;0;0;0;False;0;False;0;0;0;True;;KeywordEnum;3;Off;Alpha;Color;Create;False;True;All;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;3;-2951.248,480.5131;Inherit;False;Property;_FresnelScale;Fresnel Scale;22;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;167;-1825.17,-2162.089;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;272;-4145.222,-2313.149;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;1,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;284;-3128.085,-1079.493;Inherit;False;Property;_Line1Feature;Line 1 Feature;52;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleAddOpNode;383;-1978.61,-2174.86;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;306;-2841.692,980.8297;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;296;-2603.479,-669.2137;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;395;-1839.015,-2279.01;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;290;-4089.642,1149.74;Inherit;False;Constant;_ConstantOne;Constant One;47;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;328;-2834.318,-868.3635;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ComponentMaskNode;426;-4289.16,6726.463;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;499;-1705.199,6001.581;Inherit;False;Property;_MaskLocalFeature;Mask Local Feature;68;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;288;-3856,1104;Inherit;False;C_ONE;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;264;-3982.973,-2081.094;Inherit;False;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;396;-1594.672,-2247.686;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;305;-2577.579,1060.027;Inherit;False;Property;_FresnelFeature;Fresnel Feature;47;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;303;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;285;-2788.829,-1014.829;Inherit;False;Property;_Line2Feature;Line 2 Feature;46;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;362;-1921.947,1543.32;Inherit;False;357;vPos;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;465;-4479.439,5797.266;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;253;-1499.248,6329.262;Inherit;False;Property;_MaskFalloff;Mask Falloff;45;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;252;-1729.248,6332.262;Inherit;False;Property;_MaskSize;Mask Size;36;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;118;-2740.692,548.337;Inherit;False;116;NormalAffect;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldPosInputsNode;250;-2119.248,5828.262;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;117;-2842.344,1282.203;Inherit;False;116;NormalAffect;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;277;-3774.222,-2187.148;Inherit;False;Property;_GrainValues;Grain Values;43;0;Create;True;0;0;0;False;0;False;0,1;-1,1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.ScaleAndOffsetNode;166;-1853.036,-2466.756;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FresnelNode;2;-2735.548,333.8131;Inherit;False;Standard;WorldNormal;ViewDir;False;True;5;0;FLOAT3;0,0,1;False;4;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;327;-2470.318,-823.3635;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.Vector3Node;220;-1926.789,1679.506;Inherit;False;Property;_DissolveScale;Dissolve Scale;38;0;Create;True;0;0;0;False;0;False;0.1,1.01,5;15,0.1,0.1;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;431;-4089.615,6725.208;Inherit;False;intersectionalpha2;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;112;-2402.979,403.3258;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;286;-2408.345,-1036.021;Inherit;False;Property;_LineBothFeature;Line Both Feature;53;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.FunctionNode;249;-1377.45,5929.36;Inherit;False;BoxMask;-1;;170;9dce4093ad5a42b4aa255f0153c4f209;0;4;1;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;10;FLOAT3;0,0,0;False;17;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;221;-1683.789,1590.506;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;304;-2416.734,336.7345;Inherit;False;288;C_ONE;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;270;-4072.655,-1778.013;Inherit;False;Property;_GrainAffect;Grain Affect;40;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;261;-4247.49,5836.947;Inherit;False;intersectionalpha1;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;168;-1577.67,-2431.089;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;115;-2217.167,1114.594;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;278;-3569.222,-2024.148;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;522;-3631.864,6977.18;Inherit;False;1242.81;496;Alpha Mask;7;398;401;397;400;405;404;402;;1,1,1,1;0;0
Node;AmplifyShaderEditor.LerpOp;269;-3410.655,-2020.013;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;178;-1795.565,-2717.16;Inherit;False;Constant;_ColorRemapMinNew;Color Remap Min New;46;0;Create;True;0;0;0;False;0;False;-0.61;-0.61;-2;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;177;-1787.331,-2586.65;Inherit;False;Constant;_ColorRemapMaxNew;Color Remap Max New;47;0;Create;True;0;0;0;False;0;False;2;2;-2;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;180;-1806.597,-2974.44;Inherit;False;Constant;_ColorRemapMinOld;Color Remap Min Old;40;0;Create;True;0;0;0;False;0;False;0;0;-2;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;298;-2068.377,-973.1232;Inherit;False;line_color;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;214;-1749.202,1789.792;Inherit;False;Property;_DissolveHide;Dissolve Hide;54;0;Create;True;0;0;0;False;0;False;-1;-1;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;451;-904.1172,335.3077;Inherit;False;431;intersectionalpha2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;176;-1405.318,-2452.49;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;263;-1076.079,-160.5902;Inherit;False;261;intersectionalpha1;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;303;-2209.581,344.564;Inherit;False;Property;_FresnelFeature;Fresnel Feature;47;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;57;-2029.289,1075.339;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;212;-1461.372,1514.31;Inherit;True;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;179;-1833.266,-2820.95;Inherit;False;Constant;_ColorRemapMaxOld;Color Remap Max Old;45;0;Create;True;0;0;0;False;0;False;1;1;-2;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;237;-2550.666,631.8489;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.ClampOpNode;256;-1051.061,5957.033;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;283;-3425.69,-2110.063;Inherit;False;Constant;_Float4;Float 4;46;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;6;-1846.646,402.6709;Inherit;False;3;3;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.WireNode;441;-719.1655,-32.32913;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;500;-897.7198,6146.505;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;332;-763.3571,-166.7042;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;335;-980.3573,89.29573;Inherit;False;288;C_ONE;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;174;-1260.543,-2679.491;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;457;-573.1609,247.4301;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;299;-981.2368,-1059.819;Inherit;False;298;line_color;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;452;-807.0956,583.8937;Inherit;False;288;C_ONE;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;502;-965.7198,6268.505;Inherit;False;Property;_MaskInversion;Mask Inversion;69;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;282;-3237.69,-2033.063;Inherit;False;Property;_GrainFeature;Grain Feature;42;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;450;-590.0953,327.8937;Inherit;False;287;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;397;-3581.864,7148.637;Inherit;True;Property;_AlphaMask;Alpha Mask;63;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.RegisterLocalVarNode;434;-1878.536,1078.037;Inherit;False;fresnelalpha;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;398;-3551.054,7358.18;Inherit;False;Property;_AlphaMaskAffect;Alpha Mask Affect;64;0;Create;True;0;0;0;False;0;False;0.5;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;215;-1137.202,1610.792;Inherit;True;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;510;143.146,-228.6812;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;438;-724.0007,-535.3825;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.WireNode;439;-736.1655,-228.3291;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;401;-3300.054,7083.18;Inherit;False;288;C_ONE;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;449;-545.9037,462.2688;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;509;143.4745,-2.836522;Inherit;False;410;vertexoffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.WireNode;455;-560.9036,502.3688;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;268;-3003.289,-2093.846;Inherit;False;grain;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;453;-539.9037,705.2688;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;338;-925.9092,1536.336;Inherit;False;288;C_ONE;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;330;-516.3571,-207.7042;Inherit;False;Property;_Keyword0;Keyword 0;51;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;315;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;218;-909.2017,1625.792;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;340;-1035.714,5843.033;Inherit;False;288;C_ONE;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;245;-4244.452,5732.669;Inherit;False;intersection1;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;183;-829.1931,-2687.411;Inherit;False;Constant;_Float1;Float 1;47;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;458;-724.1306,180.5571;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;444;-734.1654,7.770939;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;443;-747.1655,80.67097;Inherit;False;261;intersectionalpha1;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;430;-4106.577,6592.93;Inherit;False;intersection2;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ComponentMaskNode;301;-748.1158,-354.381;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;435;-476.2343,-411.7932;Inherit;False;434;fresnelalpha;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;236;-453.6711,-528.7764;Inherit;False;False;False;False;True;1;0;COLOR;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;511;313.1458,104.3188;Inherit;False;Property;_Voxelization;Voxelization;70;0;Create;True;0;0;0;False;0;False;100;100;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;184;-933.1931,-2392.411;Inherit;False;Property;_ColorGlitchAffect;Color Glitch Affect;17;0;Create;True;0;0;0;False;0;False;0.5;0.3;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;501;-699.7198,6139.505;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;175;-1056.94,-2652.97;Inherit;True;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;405;-3061.054,7027.18;Inherit;False;288;C_ONE;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;436;-1633.302,415.4474;Inherit;False;fresnelcolor;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;400;-3057.054,7129.18;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;454;-573.9037,575.2689;Inherit;False;431;intersectionalpha2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;512;411.9459,-167.3812;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;447;-343.0953,286.8937;Inherit;False;Property;_Keyword0;Keyword 0;50;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;429;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;437;-477.0006,-951.3826;Inherit;False;436;fresnelcolor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;339;-674.7141,5962.033;Inherit;False;Property;_MaskFeature;Mask Feature;56;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;280;-605.4601,-2667.991;Inherit;False;Constant;_Float0;Float 0;45;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;235;-491.8311,-1042.177;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;246;-484.2697,-789.547;Inherit;False;245;intersection1;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;456;-478.5144,-626.7694;Inherit;False;430;intersection2;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleAddOpNode;58;-6.768324,-573.8707;Inherit;False;5;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;448;-253.0953,527.8937;Inherit;False;Property;_Keyword0;Keyword 0;50;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;429;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;404;-2899.054,7112.18;Inherit;False;Property;_AlphaMaskFeature;Alpha Mask Feature;65;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;334;-426.3571,33.29572;Inherit;False;Property;_Keyword0;Keyword 0;51;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;315;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;513;581.8458,-80.98119;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;271;-467.4747,-710.1898;Inherit;False;268;grain;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;337;-716.3076,1614.569;Inherit;False;Property;_DissolveFeature;Dissolve Feature;55;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;300;-488.9832,-867.1477;Inherit;False;True;True;True;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;181;-657.5865,-2563.535;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;333;133.6427,-473.7041;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RoundOpNode;514;773.6459,-88.08118;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;402;-2632.054,7099.18;Inherit;False;alphamask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;254;-419.5153,5950.769;Inherit;False;mask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;48;44.61488,-879.4854;Inherit;False;6;6;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT;0;False;5;FLOAT4;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;432;-435.5378,1646.387;Inherit;False;dissolve;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;279;-307.8887,-2551.326;Inherit;False;Property;_ColorGlitchFeature;Color Glitch Feature;41;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;516;816.7946,269.9318;Inherit;False;Property;_VoxelizationAffect;Voxelization Affect;71;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;521;492.6512,-264.8345;Inherit;False;Property;_Alpha;Alpha;73;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;255;460.0316,-418.8165;Inherit;False;254;mask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;62;302.3969,-575.8758;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;406;460.8646,-345.8843;Inherit;False;402;alphamask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;433;456.5834,-493.5936;Inherit;False;432;dissolve;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;515;895.1461,-64.6812;Inherit;True;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;185;270.725,-974.8871;Inherit;False;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ComponentMaskNode;326;517.6825,-948.8617;Inherit;False;True;True;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;224;796.1863,-614.8651;Inherit;False;5;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;517;1263.399,-145.0969;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.VertexColorNode;503;982.609,-500.9404;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StaticSwitch;518;1534.488,-224.1987;Inherit;False;Property;_VoxelizationFeature;Voxelization Feature;72;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;325;1098.498,-793.1287;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;504;1296.609,-699.9404;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;519;1903.334,-248.504;Inherit;False;ModifiedVertexPosition;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;533;1459.955,-624.184;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldPosInputsNode;131;-4782.707,3860.819;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;446;1845.364,-565.9946;Inherit;False;519;ModifiedVertexPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TransformPositionNode;140;-3488.16,4071.232;Inherit;False;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;472;1409.202,24.76617;Inherit;False;Property;_CullMode;Cull Mode;67;1;[Enum];Create;True;0;0;1;UnityEngine.Rendering.CullMode;True;0;False;2;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;459;1402.499,-79.02747;Inherit;False;Property;_ZWrite;ZWrite;66;1;[Toggle];Create;True;0;0;0;True;0;False;0;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;530;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;529;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;528;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;5;False;;10;False;;1;1;False;;10;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;True;1;True;_ZWrite;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;531;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;526;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;525;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;524;2186.873,-698.6207;Float;False;True;-1;2;Knife.HologramEffect.HologramShaderEditor;0;13;Knife/Hologram Shader Unlit;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;9;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;True;True;0;True;_CullMode;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;5;False;;10;False;;1;1;False;;10;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;True;True;2;True;_ZWrite;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForwardOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;27;Surface;1;637972967283466847;  Blend;0;0;Two Sided;1;0;Alpha Clipping;0;638701363033569914;  Use Shadow Threshold;0;0;Forward Only;0;0;Cast Shadows;0;637972967296698888;Receive Shadows;0;637972967305116547;Motion Vectors;1;0;  Add Precomputed Velocity;0;0;GPU Instancing;1;0;LOD CrossFade;0;0;Built-in Fog;0;0;Meta Pass;0;0;Extra Pre Pass;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Write Depth;0;0;  Early Z;0;0;Vertex Position,InvertActionOnDeselection;0;637972966021148095;0;11;False;True;False;True;False;False;True;True;True;False;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;523;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;527;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;532;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;534;2186.873,-598.6207;Float;False;False;-1;3;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;MotionVectors;0;10;MotionVectors;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;False;False;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=MotionVectors;False;False;0;;0;0;Standard;0;False;0
WireConnection;377;0;374;0
WireConnection;377;1;381;0
WireConnection;379;0;377;0
WireConnection;351;1;349;1
WireConnection;351;0;349;2
WireConnection;351;2;349;3
WireConnection;378;1;379;0
WireConnection;378;0;379;1
WireConnection;378;2;379;2
WireConnection;350;1;348;1
WireConnection;350;0;348;2
WireConnection;350;2;348;3
WireConnection;352;1;350;0
WireConnection;352;0;351;0
WireConnection;352;2;378;0
WireConnection;367;0;352;0
WireConnection;367;1;368;0
WireConnection;497;0;367;0
WireConnection;385;0;384;0
WireConnection;353;0;497;0
WireConnection;149;0;150;0
WireConnection;394;0;149;0
WireConnection;394;1;393;0
WireConnection;155;0;154;0
WireConnection;152;0;364;0
WireConnection;152;1;151;0
WireConnection;152;2;394;0
WireConnection;287;0;289;0
WireConnection;392;0;393;0
WireConnection;392;1;155;0
WireConnection;141;0;373;1
WireConnection;141;1;373;2
WireConnection;141;2;373;3
WireConnection;139;0;138;0
WireConnection;153;0;152;0
WireConnection;153;1;392;0
WireConnection;157;0;156;0
WireConnection;208;0;153;0
WireConnection;365;1;141;0
WireConnection;365;0;366;0
WireConnection;388;0;139;0
WireConnection;388;1;387;0
WireConnection;142;0;365;0
WireConnection;142;1;137;0
WireConnection;142;2;388;0
WireConnection;190;0;208;0
WireConnection;389;0;391;0
WireConnection;389;1;157;0
WireConnection;194;0;193;0
WireConnection;194;1;190;0
WireConnection;158;0;142;0
WireConnection;158;1;389;0
WireConnection;195;0;194;0
WireConnection;195;1;190;1
WireConnection;187;0;158;0
WireConnection;226;0;187;0
WireConnection;226;1;225;0
WireConnection;188;0;195;0
WireConnection;147;0;226;0
WireConnection;147;1;126;0
WireConnection;147;2;127;0
WireConnection;147;3;202;0
WireConnection;147;4;129;0
WireConnection;186;0;153;0
WireConnection;191;0;188;0
WireConnection;191;1;126;0
WireConnection;191;2;127;0
WireConnection;191;3;202;0
WireConnection;191;4;129;0
WireConnection;148;0;147;0
WireConnection;192;0;191;0
WireConnection;124;0;186;0
WireConnection;124;1;126;0
WireConnection;124;2;127;0
WireConnection;124;3;202;0
WireConnection;124;4;129;0
WireConnection;199;1;192;0
WireConnection;199;2;198;0
WireConnection;197;0;124;0
WireConnection;197;1;148;0
WireConnection;94;0;84;0
WireConnection;201;0;197;0
WireConnection;201;1;199;0
WireConnection;122;0;121;0
WireConnection;196;0;197;0
WireConnection;196;1;201;0
WireConnection;506;0;122;0
WireConnection;506;1;505;0
WireConnection;508;0;94;0
WireConnection;508;1;507;0
WireConnection;537;37;89;0
WireConnection;537;44;479;0
WireConnection;537;43;363;0
WireConnection;537;13;88;0
WireConnection;537;14;90;0
WireConnection;537;15;87;0
WireConnection;537;16;86;0
WireConnection;123;0;506;0
WireConnection;123;1;196;0
WireConnection;123;2;128;0
WireConnection;81;0;508;0
WireConnection;81;1;537;0
WireConnection;311;1;312;0
WireConnection;311;0;123;0
WireConnection;307;1;308;0
WireConnection;307;0;81;0
WireConnection;491;0;307;0
WireConnection;493;0;311;0
WireConnection;159;0;492;0
WireConnection;159;1;494;0
WireConnection;410;0;159;0
WireConnection;416;0;415;0
WireConnection;416;1;414;0
WireConnection;411;0;409;0
WireConnection;411;1;412;0
WireConnection;95;5;100;0
WireConnection;101;0;95;0
WireConnection;101;1;102;0
WireConnection;417;1;416;0
WireConnection;417;0;427;0
WireConnection;238;1;411;0
WireConnection;238;0;239;0
WireConnection;232;0;231;0
WireConnection;242;0;238;0
WireConnection;114;0;101;0
WireConnection;418;0;417;0
WireConnection;535;37;77;0
WireConnection;535;44;480;0
WireConnection;535;43;354;0
WireConnection;535;13;71;0
WireConnection;535;14;72;0
WireConnection;535;15;73;0
WireConnection;535;16;74;0
WireConnection;536;37;37;0
WireConnection;536;44;481;0
WireConnection;536;43;355;0
WireConnection;536;13;10;0
WireConnection;536;14;15;0
WireConnection;536;15;13;0
WireConnection;536;16;24;0
WireConnection;244;0;247;0
WireConnection;244;1;242;0
WireConnection;244;2;248;0
WireConnection;244;3;461;0
WireConnection;106;1;114;0
WireConnection;106;2;104;0
WireConnection;344;0;238;0
WireConnection;344;1;248;0
WireConnection;70;0;233;0
WireConnection;70;1;535;0
WireConnection;420;0;419;0
WireConnection;420;1;418;0
WireConnection;420;2;428;0
WireConnection;420;3;463;0
WireConnection;422;0;417;0
WireConnection;422;1;428;0
WireConnection;35;0;234;0
WireConnection;35;1;536;0
WireConnection;294;0;70;0
WireConnection;467;1;344;0
WireConnection;467;2;461;0
WireConnection;69;0;535;0
WireConnection;69;1;75;0
WireConnection;380;0;377;0
WireConnection;293;0;35;0
WireConnection;113;0;106;0
WireConnection;421;0;420;0
WireConnection;313;0;244;0
WireConnection;460;0;418;0
WireConnection;460;1;428;0
WireConnection;460;2;463;0
WireConnection;469;0;242;0
WireConnection;469;1;248;0
WireConnection;469;2;461;0
WireConnection;471;1;422;0
WireConnection;471;2;463;0
WireConnection;61;0;536;0
WireConnection;61;1;60;0
WireConnection;375;1;348;0
WireConnection;375;0;349;0
WireConnection;375;2;380;0
WireConnection;228;0;293;0
WireConnection;228;1;294;0
WireConnection;162;0;161;1
WireConnection;162;1;161;2
WireConnection;162;2;161;3
WireConnection;314;0;313;0
WireConnection;314;3;469;0
WireConnection;165;0;160;0
WireConnection;424;3;471;0
WireConnection;321;3;467;0
WireConnection;341;1;342;0
WireConnection;341;0;113;0
WireConnection;425;0;421;0
WireConnection;425;3;460;0
WireConnection;229;0;61;0
WireConnection;229;1;69;0
WireConnection;292;0;293;0
WireConnection;292;3;61;0
WireConnection;53;2;54;0
WireConnection;53;3;55;0
WireConnection;360;1;162;0
WireConnection;360;0;361;0
WireConnection;315;1;316;0
WireConnection;315;0;321;0
WireConnection;315;2;314;0
WireConnection;116;0;341;0
WireConnection;258;0;251;0
WireConnection;258;1;257;0
WireConnection;295;0;294;0
WireConnection;295;3;69;0
WireConnection;357;0;375;0
WireConnection;429;1;423;0
WireConnection;429;0;424;0
WireConnection;429;2;425;0
WireConnection;167;0;164;0
WireConnection;272;0;265;0
WireConnection;272;1;267;0
WireConnection;272;2;273;0
WireConnection;284;1;291;0
WireConnection;284;0;292;0
WireConnection;383;0;386;0
WireConnection;383;1;165;0
WireConnection;296;0;228;0
WireConnection;296;3;229;0
WireConnection;328;3;61;0
WireConnection;426;0;429;0
WireConnection;499;1;251;0
WireConnection;499;0;258;0
WireConnection;288;0;290;0
WireConnection;264;0;272;0
WireConnection;396;0;395;0
WireConnection;396;1;167;0
WireConnection;305;1;306;0
WireConnection;305;0;53;0
WireConnection;285;1;284;0
WireConnection;285;0;295;0
WireConnection;465;0;315;0
WireConnection;166;0;360;0
WireConnection;166;1;163;0
WireConnection;166;2;383;0
WireConnection;2;2;3;0
WireConnection;2;3;4;0
WireConnection;327;0;328;0
WireConnection;327;1;296;0
WireConnection;431;0;426;0
WireConnection;112;0;2;0
WireConnection;112;1;118;0
WireConnection;286;1;285;0
WireConnection;286;0;327;0
WireConnection;249;1;250;0
WireConnection;249;4;499;0
WireConnection;249;10;252;0
WireConnection;249;17;253;0
WireConnection;221;0;362;0
WireConnection;221;1;220;0
WireConnection;261;0;465;0
WireConnection;168;0;166;0
WireConnection;168;1;396;0
WireConnection;115;0;305;0
WireConnection;115;1;117;0
WireConnection;278;0;277;1
WireConnection;278;1;277;2
WireConnection;278;2;264;0
WireConnection;269;1;278;0
WireConnection;269;2;270;0
WireConnection;298;0;286;0
WireConnection;176;0;168;0
WireConnection;303;1;304;0
WireConnection;303;0;112;0
WireConnection;57;0;115;0
WireConnection;212;0;221;0
WireConnection;256;0;249;0
WireConnection;6;0;303;0
WireConnection;6;1;237;0
WireConnection;6;2;57;0
WireConnection;441;0;263;0
WireConnection;500;0;256;0
WireConnection;174;0;176;0
WireConnection;174;1;180;0
WireConnection;174;2;179;0
WireConnection;174;3;178;0
WireConnection;174;4;177;0
WireConnection;457;0;451;0
WireConnection;282;1;283;0
WireConnection;282;0;269;0
WireConnection;434;0;57;0
WireConnection;215;0;212;0
WireConnection;215;1;214;0
WireConnection;439;0;263;0
WireConnection;449;0;451;0
WireConnection;455;0;452;0
WireConnection;268;0;282;0
WireConnection;453;0;452;0
WireConnection;330;1;439;0
WireConnection;330;0;332;0
WireConnection;330;2;441;0
WireConnection;218;0;215;0
WireConnection;245;0;315;0
WireConnection;458;0;335;0
WireConnection;444;0;335;0
WireConnection;430;0;429;0
WireConnection;301;0;299;0
WireConnection;236;0;438;0
WireConnection;501;0;256;0
WireConnection;501;1;500;0
WireConnection;501;2;502;0
WireConnection;175;0;174;0
WireConnection;436;0;6;0
WireConnection;400;0;401;0
WireConnection;400;1;397;1
WireConnection;400;2;398;0
WireConnection;512;0;510;0
WireConnection;512;1;509;0
WireConnection;447;1;457;0
WireConnection;447;0;450;0
WireConnection;447;2;449;0
WireConnection;339;1;340;0
WireConnection;339;0;501;0
WireConnection;58;0;236;0
WireConnection;58;1;435;0
WireConnection;58;2;301;0
WireConnection;58;3;330;0
WireConnection;58;4;447;0
WireConnection;448;1;455;0
WireConnection;448;0;454;0
WireConnection;448;2;453;0
WireConnection;404;1;405;0
WireConnection;404;0;400;0
WireConnection;334;1;444;0
WireConnection;334;0;443;0
WireConnection;334;2;458;0
WireConnection;513;0;512;0
WireConnection;513;1;511;0
WireConnection;337;1;338;0
WireConnection;337;0;218;0
WireConnection;300;0;299;0
WireConnection;181;0;183;0
WireConnection;181;1;175;0
WireConnection;181;2;184;0
WireConnection;333;0;58;0
WireConnection;333;1;334;0
WireConnection;333;2;448;0
WireConnection;514;0;513;0
WireConnection;402;0;404;0
WireConnection;254;0;339;0
WireConnection;48;0;235;0
WireConnection;48;1;437;0
WireConnection;48;2;300;0
WireConnection;48;3;246;0
WireConnection;48;4;271;0
WireConnection;48;5;456;0
WireConnection;432;0;337;0
WireConnection;279;1;280;0
WireConnection;279;0;181;0
WireConnection;62;0;333;0
WireConnection;515;0;514;0
WireConnection;515;1;511;0
WireConnection;185;0;279;0
WireConnection;185;1;48;0
WireConnection;326;0;185;0
WireConnection;224;0;62;0
WireConnection;224;1;433;0
WireConnection;224;2;255;0
WireConnection;224;3;406;0
WireConnection;224;4;521;0
WireConnection;517;0;512;0
WireConnection;517;1;515;0
WireConnection;517;2;516;0
WireConnection;518;1;512;0
WireConnection;518;0;517;0
WireConnection;325;0;326;0
WireConnection;325;3;224;0
WireConnection;504;0;325;0
WireConnection;504;1;503;0
WireConnection;519;0;518;0
WireConnection;533;0;504;0
WireConnection;524;2;504;0
WireConnection;524;3;533;0
WireConnection;524;5;446;0
ASEEND*/
//CHKSM=9D401F311850EB85CA800B8C5BAE71DB27CFB397