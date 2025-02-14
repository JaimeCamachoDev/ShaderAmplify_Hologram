// Made with Amplify Shader Editor v1.9.8.1
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Hologram Shader IA"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[HDR]_MainColor("Main Color", Color) = (0.620945,1.420074,3.953349,0.05098039)
		_MainTexture("Main Texture", 2D) = "white" {}
		_Alpha("Alpha", Range( 0 , 1)) = 0
		_NormalMap("NormalMap", 2D) = "bump" {}
		_NormalScale("NormalScale", Float) = 0
		_NormalAffect("NormalAffect", Range( 0 , 1)) = 0
		[Header(FRESNEL)][Header(_)]_FresnelScale("Fresnel Scale", Float) = 1
		_FresnelPower("Fresnel Power", Float) = 2
		_FresnelAlphaScale("Fresnel Alpha Scale", Float) = 1
		_FresnelAlphaPower("Fresnel Alpha Power", Float) = 2
		[Header(Lines)][Header(_)][NoScaleOffset]_Line3("Line 1", 2D) = "white" {}
		_Line1Speed1("Line 1 Speed", Float) = -3.57
		_Line1Frequency1("Line 1 Frequency", Float) = 100
		_Line1Hardness1("Line 1 Hardness", Float) = 1.45
		_Line1InvertedThickness1("Line 1 Inverted Thickness", Range( 0 , 1)) = 0
		_Line1Alpha1("Line 1 Alpha", Float) = 0.15
		_Line2Speed("Line 2 Speed", Float) = -1
		_Line2Alpha("Line 2 Alpha", Float) = 0.1
		[Toggle(_GRAINFEATURE_ON)] _GrainFeature("Grain Feature", Float) = 0
		[Header(Grain)][Header(_)]_GrainAffect("Grain Affect", Range( 0 , 1)) = 1
		_GrainScale("Grain Scale", Int) = 1000
		_ColorGlitchAffect("Color Glitch Affect", Range( 0 , 1)) = 0.5
		[KeywordEnum(X,Y,Z)] _PositionFeature("Position Feature", Float) = 1
		_PositionDirection("Position Direction", Float) = 1
		[Header(Glitch)][NoScaleOffset]_LineGlitch("Line Glitch", 2D) = "white" {}
		_LineGlitchOffset("Line Glitch Offset", Vector) = (0.03,0,0,0)
		_RandomGlitchOffset("Random Glitch Offset", Vector) = (-0.5,0,0,0)
		_RandomGlitchAmount("Random Glitch Amount", Range( 0 , 1)) = 0.089
		_LineGlitchSpeed("Line Glitch Speed", Float) = -0.26
		_LineGlitchFrequency("Line Glitch Frequency", Float) = 0.2
		_LineGlitchHardness("Line Glitch Hardness", Float) = 5
		_LineGlitchInvertedThickness("Line Glitch Inverted Thickness", Range( 0 , 1)) = 0.825
		_RandomGlitchConstant("Random Glitch Constant", Range( 0 , 1)) = 0
		_RandomGlitchTiling("Random Glitch Tiling", Float) = 2.83
		[Toggle(_LINEGLITCHFEATURE_ON)] _LineGlitchFeature("Line Glitch Feature", Float) = 0
		[Toggle(_RANDOMGLITCHFEATURE_ON)] _RandomGlitchFeature("Random Glitch Feature", Float) = 0
		[KeywordEnum(World,Local,Custom)] _PositionSpaceFeature("Position Space Feature", Float) = 0
		_RandomOffset("Random Offset", Float) = 0
		_Voxelization("Voxelization", Float) = 100
		_VoxelizationAffect("Voxelization Affect", Range( 0 , 1)) = 1
		[Toggle(_VOXELIZATIONFEATURE_ON)] _VoxelizationFeature("Voxelization Feature", Float) = 0
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

		Cull Off
		AlphaToMask Off

		Stencil
		{
			Ref 255
			CompFront Always
			PassFront Keep
			FailFront Keep
			ZFailFront Keep
			CompBack Always
			PassBack Keep
			FailBack Keep
			ZFailBack Keep
		}

		HLSLINCLUDE
		#pragma target 4.5
		#pragma prefer_hlslcc gles
		#pragma exclude_renderers xboxone xboxseries playstation ps4 ps5 switch // ensure rendering platforms toggle list is visible

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
			Tags { "LightMode"="UniversalForward" }

			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite On
			ZTest LEqual
			Offset 0,0
			ColorMask RGBA

			

			HLSLPROGRAM

			#pragma instancing_options renderinglayer
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_VERSION 19801
			#define ASE_SRP_VERSION 170003


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
			#pragma shader_feature _VOXELIZATIONFEATURE_ON
			#pragma shader_feature _LINEGLITCHFEATURE_ON
			#pragma shader_feature _POSITIONSPACEFEATURE_WORLD _POSITIONSPACEFEATURE_LOCAL _POSITIONSPACEFEATURE_CUSTOM
			#pragma shader_feature _POSITIONFEATURE_X _POSITIONFEATURE_Y _POSITIONFEATURE_Z
			#pragma multi_compile_instancing
			#pragma shader_feature _RANDOMGLITCHFEATURE_ON
			#pragma shader_feature _GRAINFEATURE_ON


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
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_tangent : TANGENT;
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
				#if defined(LIGHTMAP_ON)
					float4 lightmapUVOrVertexSH : TEXCOORD4;
				#endif
				#if defined(DYNAMICLIGHTMAP_ON)
					float2 dynamicLightmapUV : TEXCOORD5;
				#endif
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_texcoord8 : TEXCOORD8;
				float4 ase_texcoord9 : TEXCOORD9;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainColor;
			float3 _LineGlitchOffset;
			float3 _RandomGlitchOffset;
			float _Line2Speed;
			float _Line1Alpha1;
			float _Line1Hardness1;
			float _Line1InvertedThickness1;
			float _Line1Speed1;
			float _Line1Frequency1;
			float _GrainAffect;
			int _GrainScale;
			float _FresnelAlphaPower;
			float _FresnelAlphaScale;
			float _NormalAffect;
			float _NormalScale;
			float _FresnelPower;
			float _FresnelScale;
			float _ColorGlitchAffect;
			float _VoxelizationAffect;
			float _Voxelization;
			float _RandomGlitchAmount;
			float _RandomGlitchConstant;
			float _RandomGlitchTiling;
			float _LineGlitchHardness;
			float _LineGlitchInvertedThickness;
			float _RandomOffset;
			float _LineGlitchSpeed;
			float _LineGlitchFrequency;
			float _PositionDirection;
			float _Line2Alpha;
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
			sampler2D _MainTexture;
			sampler2D _Line3;
			UNITY_INSTANCING_BUFFER_START(HologramShaderIA)
				UNITY_DEFINE_INSTANCED_PROP(float4x4, _CustomMatrix)
				UNITY_DEFINE_INSTANCED_PROP(float4, _NormalMap_ST)
				UNITY_DEFINE_INSTANCED_PROP(float4, _MainTexture_ST)
			UNITY_INSTANCING_BUFFER_END(HologramShaderIA)


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

				float3 temp_cast_0 = ((0)).xxx;
				float3 viewToObjDir94 = mul( UNITY_MATRIX_T_MV, float4( _LineGlitchOffset, 0.0 ) ).xyz;
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float3 ase_positionWS = TransformObjectToWorld( ( input.positionOS ).xyz );
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
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(HologramShaderIA,_CustomMatrix);
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
				float mulTime3_g175 = _TimeParameters.x * _LineGlitchSpeed;
				float randomoffset385 = _RandomOffset;
				float2 temp_cast_3 = ((pos353*_LineGlitchFrequency + ( mulTime3_g175 + randomoffset385 ))).xx;
				float clampResult42_g175 = clamp( ( ( tex2Dlod( _LineGlitch, float4( temp_cast_3, 0, 0.0) ).r - _LineGlitchInvertedThickness ) * _LineGlitchHardness ) , 0.0 , 1.0 );
				#ifdef _LINEGLITCHFEATURE_ON
				float3 staticSwitch307 = ( ( viewToObjDir94 / ase_objectScale ) * clampResult42_g175 );
				#else
				float3 staticSwitch307 = temp_cast_0;
				#endif
				float3 lineglitch491 = staticSwitch307;
				float3 temp_cast_4 = ((0)).xxx;
				float3 viewToObjDir122 = mul( UNITY_MATRIX_T_MV, float4( _RandomGlitchOffset, 0.0 ) ).xyz;
				float mulTime149 = _TimeParameters.x * -2.3;
				float mulTime155 = _TimeParameters.x * -2.05;
				float2 appendResult153 = (float2((pos353*_RandomGlitchTiling + ( mulTime149 + randomoffset385 )) , ( randomoffset385 + mulTime155 )));
				float simplePerlin2D186 = snoise( appendResult153 );
				simplePerlin2D186 = simplePerlin2D186*0.5 + 0.5;
				float4 matrixToPos373 = float4( float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[0][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[1][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[2][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[3][3]);
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch365 = (0);
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
				
				float3 ase_normalWS = TransformObjectToWorldNormal( input.normalOS );
				output.ase_texcoord6.xyz = ase_normalWS;
				float3 ase_tangentWS = TransformObjectToWorldDir( input.ase_tangent.xyz );
				output.ase_texcoord8.xyz = ase_tangentWS;
				float ase_tangentSign = input.ase_tangent.w * ( unity_WorldTransformParams.w >= 0.0 ? 1.0 : -1.0 );
				float3 ase_bitangentWS = cross( ase_normalWS, ase_tangentWS ) * ase_tangentSign;
				output.ase_texcoord9.xyz = ase_bitangentWS;
				
				output.ase_texcoord7.xy = input.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord6.w = 0;
				output.ase_texcoord7.zw = 0;
				output.ase_texcoord8.w = 0;
				output.ase_texcoord9.w = 0;

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

				#if defined(LIGHTMAP_ON)
					OUTPUT_LIGHTMAP_UV(input.texcoord1, unity_LightmapST, output.lightmapUVOrVertexSH.xy);
				#endif
				#if defined(DYNAMICLIGHTMAP_ON)
					output.dynamicLightmapUV.xy = input.texcoord2.xy * unity_DynamicLightmapST.xy + unity_DynamicLightmapST.zw;
				#endif

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
				float4 ase_tangent : TANGENT;

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
				output.ase_tangent = input.ase_tangent;
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
				output.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
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
				float3 WorldViewDirection = GetWorldSpaceNormalizeViewDir( WorldPosition );
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

				float randomoffset385 = _RandomOffset;
				float mulTime165 = _TimeParameters.x * -15.0;
				float mulTime167 = _TimeParameters.x * -0.5;
				float2 appendResult168 = (float2((0.0*223.0 + ( randomoffset385 + mulTime165 )) , ( randomoffset385 + mulTime167 )));
				float simplePerlin2D176 = snoise( appendResult168 );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float clampResult175 = clamp( (-0.61 + (simplePerlin2D176 - 0.0) * (2.0 - -0.61) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult181 = lerp( 1.0 , clampResult175 , _ColorGlitchAffect);
				float ColorGlitch689 = lerpResult181;
				float3 ase_normalWS = input.ase_texcoord6.xyz;
				float fresnelNdotV571 = dot( ase_normalWS, WorldViewDirection );
				float fresnelNode571 = ( 0.0 + _FresnelScale * pow( max( 1.0 - fresnelNdotV571 , 0.0001 ), _FresnelPower ) );
				float4 _NormalMap_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(HologramShaderIA,_NormalMap_ST);
				float2 uv_NormalMap = input.ase_texcoord7.xy * _NormalMap_ST_Instance.xy + _NormalMap_ST_Instance.zw;
				float3 unpack584 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalScale );
				unpack584.z = lerp( 1, unpack584.z, saturate(_NormalScale) );
				float3 ase_tangentWS = input.ase_texcoord8.xyz;
				float3 ase_bitangentWS = input.ase_texcoord9.xyz;
				float3 tanToWorld0 = float3( ase_tangentWS.x, ase_bitangentWS.x, ase_normalWS.x );
				float3 tanToWorld1 = float3( ase_tangentWS.y, ase_bitangentWS.y, ase_normalWS.y );
				float3 tanToWorld2 = float3( ase_tangentWS.z, ase_bitangentWS.z, ase_normalWS.z );
				float3 ase_viewVectorTS =  tanToWorld0 * ( _WorldSpaceCameraPos.xyz - WorldPosition ).x + tanToWorld1 * ( _WorldSpaceCameraPos.xyz - WorldPosition ).y  + tanToWorld2 * ( _WorldSpaceCameraPos.xyz - WorldPosition ).z;
				float3 ase_viewDirTS = normalize( ase_viewVectorTS );
				float dotResult587 = dot( unpack584 , ase_viewDirTS );
				float lerpResult590 = lerp( 1.0 , (0.0 + (dotResult587 - -1.0) * (1.0 - 0.0) / (1.0 - -1.0)) , _NormalAffect);
				float NormalAffect594 = ( 1.0 - lerpResult590 );
				float4 _MainTexture_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(HologramShaderIA,_MainTexture_ST);
				float2 uv_MainTexture = input.ase_texcoord7.xy * _MainTexture_ST_Instance.xy + _MainTexture_ST_Instance.zw;
				float4 tex2DNode605 = tex2D( _MainTexture, uv_MainTexture );
				float3 maincolor232 = ( _MainColor.rgb * tex2DNode605.rgb );
				float fresnelNdotV569 = dot( ase_normalWS, WorldViewDirection );
				float fresnelNode569 = ( 0.0 + _FresnelAlphaScale * pow( max( 1.0 - fresnelNdotV569 , 0.0001 ), _FresnelAlphaPower ) );
				float clampResult577 = clamp( ( fresnelNode569 + NormalAffect594 ) , 0.0 , 1.0 );
				float3 fresnelcolor580 = ( ( fresnelNode571 + NormalAffect594 ) * maincolor232 * clampResult577 );
				float3 temp_cast_0 = _GrainScale;
				float mulTime273 = _TimeParameters.x * 10.0;
				float simplePerlin3D264 = snoise( (WorldPosition*temp_cast_0 + mulTime273) );
				simplePerlin3D264 = simplePerlin3D264*0.5 + 0.5;
				float lerpResult278 = lerp( -1.0 , 1.0 , simplePerlin3D264);
				float lerpResult269 = lerp( 0.0 , lerpResult278 , _GrainAffect);
				#ifdef _GRAINFEATURE_ON
				float staticSwitch282 = lerpResult269;
				#else
				float staticSwitch282 = 0.0;
				#endif
				float grain268 = staticSwitch282;
				float mulTime3_g173 = _TimeParameters.x * _Line1Speed1;
				float2 temp_cast_1 = ((WorldPosition.y*_Line1Frequency1 + ( mulTime3_g173 + randomoffset385 ))).xx;
				float clampResult42_g173 = clamp( ( ( tex2D( _Line3, temp_cast_1 ).r - _Line1InvertedThickness1 ) * _Line1Hardness1 ) , 0.0 , 1.0 );
				float temp_output_629_0 = clampResult42_g173;
				float temp_output_638_0 = ( temp_output_629_0 * _Line1Alpha1 );
				float4 appendResult683 = (float4(0.0 , 0.0 , 0.0 , temp_output_638_0));
				float mulTime685 = _TimeParameters.x * _Line2Speed;
				float temp_output_688_0 = frac( ( WorldPosition.y + mulTime685 ) );
				float4 appendResult644 = (float4(( (( maincolor232 * temp_output_629_0 )).xyz * (( maincolor232 * temp_output_688_0 )).xyz ) , ( temp_output_638_0 * ( temp_output_688_0 * _Line2Alpha ) )));
				float4 line_color650 = ( appendResult683 + appendResult644 );
				float C_ZERO596 = 0.0;
				float lerpResult400 = lerp( C_ZERO596 , tex2DNode605.a , _Alpha);
				float alphamask402 = lerpResult400;
				float fresnelalpha579 = clampResult577;
				float clampResult677 = clamp( ( fresnelalpha579 + (line_color650).w ) , 0.0 , 1.0 );
				float4 appendResult679 = (float4(( ColorGlitch689 * ( fresnelcolor580 + grain268 + maincolor232 + (line_color650).xyz ) ) , ( alphamask402 * clampResult677 )));
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = appendResult679.xyz;
				float Alpha = (appendResult679).w;
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
			
			Name "MotionVectors"
			Tags { "LightMode"="MotionVectors" }

			ColorMask RG

			HLSLPROGRAM

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_VERSION 19801
			#define ASE_SRP_VERSION 170003


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
			#pragma shader_feature _VOXELIZATIONFEATURE_ON
			#pragma shader_feature _LINEGLITCHFEATURE_ON
			#pragma shader_feature _POSITIONSPACEFEATURE_WORLD _POSITIONSPACEFEATURE_LOCAL _POSITIONSPACEFEATURE_CUSTOM
			#pragma shader_feature _POSITIONFEATURE_X _POSITIONFEATURE_Y _POSITIONFEATURE_Z
			#pragma multi_compile_instancing
			#pragma shader_feature _RANDOMGLITCHFEATURE_ON
			#pragma shader_feature _GRAINFEATURE_ON


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
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainColor;
			float3 _LineGlitchOffset;
			float3 _RandomGlitchOffset;
			float _Line2Speed;
			float _Line1Alpha1;
			float _Line1Hardness1;
			float _Line1InvertedThickness1;
			float _Line1Speed1;
			float _Line1Frequency1;
			float _GrainAffect;
			int _GrainScale;
			float _FresnelAlphaPower;
			float _FresnelAlphaScale;
			float _NormalAffect;
			float _NormalScale;
			float _FresnelPower;
			float _FresnelScale;
			float _ColorGlitchAffect;
			float _VoxelizationAffect;
			float _Voxelization;
			float _RandomGlitchAmount;
			float _RandomGlitchConstant;
			float _RandomGlitchTiling;
			float _LineGlitchHardness;
			float _LineGlitchInvertedThickness;
			float _RandomOffset;
			float _LineGlitchSpeed;
			float _LineGlitchFrequency;
			float _PositionDirection;
			float _Line2Alpha;
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
			sampler2D _MainTexture;
			sampler2D _Line3;
			UNITY_INSTANCING_BUFFER_START(HologramShaderIA)
				UNITY_DEFINE_INSTANCED_PROP(float4x4, _CustomMatrix)
				UNITY_DEFINE_INSTANCED_PROP(float4, _NormalMap_ST)
				UNITY_DEFINE_INSTANCED_PROP(float4, _MainTexture_ST)
			UNITY_INSTANCING_BUFFER_END(HologramShaderIA)


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

				float3 temp_cast_0 = ((0)).xxx;
				float3 viewToObjDir94 = mul( UNITY_MATRIX_T_MV, float4( _LineGlitchOffset, 0.0 ) ).xyz;
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float3 ase_positionWS = TransformObjectToWorld( ( input.positionOS ).xyz );
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
				float4x4 _CustomMatrix_Instance = UNITY_ACCESS_INSTANCED_PROP(HologramShaderIA,_CustomMatrix);
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
				float mulTime3_g175 = _TimeParameters.x * _LineGlitchSpeed;
				float randomoffset385 = _RandomOffset;
				float2 temp_cast_3 = ((pos353*_LineGlitchFrequency + ( mulTime3_g175 + randomoffset385 ))).xx;
				float clampResult42_g175 = clamp( ( ( tex2Dlod( _LineGlitch, float4( temp_cast_3, 0, 0.0) ).r - _LineGlitchInvertedThickness ) * _LineGlitchHardness ) , 0.0 , 1.0 );
				#ifdef _LINEGLITCHFEATURE_ON
				float3 staticSwitch307 = ( ( viewToObjDir94 / ase_objectScale ) * clampResult42_g175 );
				#else
				float3 staticSwitch307 = temp_cast_0;
				#endif
				float3 lineglitch491 = staticSwitch307;
				float3 temp_cast_4 = ((0)).xxx;
				float3 viewToObjDir122 = mul( UNITY_MATRIX_T_MV, float4( _RandomGlitchOffset, 0.0 ) ).xyz;
				float mulTime149 = _TimeParameters.x * -2.3;
				float mulTime155 = _TimeParameters.x * -2.05;
				float2 appendResult153 = (float2((pos353*_RandomGlitchTiling + ( mulTime149 + randomoffset385 )) , ( randomoffset385 + mulTime155 )));
				float simplePerlin2D186 = snoise( appendResult153 );
				simplePerlin2D186 = simplePerlin2D186*0.5 + 0.5;
				float4 matrixToPos373 = float4( float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[0][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[1][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[2][3],float4x4( 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1 )[3][3]);
				#if defined( _POSITIONSPACEFEATURE_WORLD )
				float staticSwitch365 = ( matrixToPos373.x + matrixToPos373.y + matrixToPos373.z );
				#elif defined( _POSITIONSPACEFEATURE_LOCAL )
				float staticSwitch365 = (0);
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
				float3 ase_normalWS = TransformObjectToWorldNormal( input.ase_normal );
				output.ase_texcoord3.xyz = ase_normalWS;
				float3 ase_tangentWS = TransformObjectToWorldDir( input.ase_tangent.xyz );
				output.ase_texcoord5.xyz = ase_tangentWS;
				float ase_tangentSign = input.ase_tangent.w * ( unity_WorldTransformParams.w >= 0.0 ? 1.0 : -1.0 );
				float3 ase_bitangentWS = cross( ase_normalWS, ase_tangentWS ) * ase_tangentSign;
				output.ase_texcoord6.xyz = ase_bitangentWS;
				
				output.ase_texcoord4.xy = input.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord2.w = 0;
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

				float randomoffset385 = _RandomOffset;
				float mulTime165 = _TimeParameters.x * -15.0;
				float mulTime167 = _TimeParameters.x * -0.5;
				float2 appendResult168 = (float2((0.0*223.0 + ( randomoffset385 + mulTime165 )) , ( randomoffset385 + mulTime167 )));
				float simplePerlin2D176 = snoise( appendResult168 );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float clampResult175 = clamp( (-0.61 + (simplePerlin2D176 - 0.0) * (2.0 - -0.61) / (1.0 - 0.0)) , 0.0 , 1.0 );
				float lerpResult181 = lerp( 1.0 , clampResult175 , _ColorGlitchAffect);
				float ColorGlitch689 = lerpResult181;
				float3 ase_positionWS = input.ase_texcoord2.xyz;
				float3 ase_viewVectorWS = ( _WorldSpaceCameraPos.xyz - ase_positionWS );
				float3 ase_viewDirWS = normalize( ase_viewVectorWS );
				float3 ase_normalWS = input.ase_texcoord3.xyz;
				float fresnelNdotV571 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode571 = ( 0.0 + _FresnelScale * pow( max( 1.0 - fresnelNdotV571 , 0.0001 ), _FresnelPower ) );
				float4 _NormalMap_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(HologramShaderIA,_NormalMap_ST);
				float2 uv_NormalMap = input.ase_texcoord4.xy * _NormalMap_ST_Instance.xy + _NormalMap_ST_Instance.zw;
				float3 unpack584 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalScale );
				unpack584.z = lerp( 1, unpack584.z, saturate(_NormalScale) );
				float3 ase_tangentWS = input.ase_texcoord5.xyz;
				float3 ase_bitangentWS = input.ase_texcoord6.xyz;
				float3 tanToWorld0 = float3( ase_tangentWS.x, ase_bitangentWS.x, ase_normalWS.x );
				float3 tanToWorld1 = float3( ase_tangentWS.y, ase_bitangentWS.y, ase_normalWS.y );
				float3 tanToWorld2 = float3( ase_tangentWS.z, ase_bitangentWS.z, ase_normalWS.z );
				float3 ase_viewVectorTS =  tanToWorld0 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).x + tanToWorld1 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).y  + tanToWorld2 * ( _WorldSpaceCameraPos.xyz - ase_positionWS ).z;
				float3 ase_viewDirTS = normalize( ase_viewVectorTS );
				float dotResult587 = dot( unpack584 , ase_viewDirTS );
				float lerpResult590 = lerp( 1.0 , (0.0 + (dotResult587 - -1.0) * (1.0 - 0.0) / (1.0 - -1.0)) , _NormalAffect);
				float NormalAffect594 = ( 1.0 - lerpResult590 );
				float4 _MainTexture_ST_Instance = UNITY_ACCESS_INSTANCED_PROP(HologramShaderIA,_MainTexture_ST);
				float2 uv_MainTexture = input.ase_texcoord4.xy * _MainTexture_ST_Instance.xy + _MainTexture_ST_Instance.zw;
				float4 tex2DNode605 = tex2D( _MainTexture, uv_MainTexture );
				float3 maincolor232 = ( _MainColor.rgb * tex2DNode605.rgb );
				float fresnelNdotV569 = dot( ase_normalWS, ase_viewDirWS );
				float fresnelNode569 = ( 0.0 + _FresnelAlphaScale * pow( max( 1.0 - fresnelNdotV569 , 0.0001 ), _FresnelAlphaPower ) );
				float clampResult577 = clamp( ( fresnelNode569 + NormalAffect594 ) , 0.0 , 1.0 );
				float3 fresnelcolor580 = ( ( fresnelNode571 + NormalAffect594 ) * maincolor232 * clampResult577 );
				float3 temp_cast_0 = _GrainScale;
				float mulTime273 = _TimeParameters.x * 10.0;
				float simplePerlin3D264 = snoise( (ase_positionWS*temp_cast_0 + mulTime273) );
				simplePerlin3D264 = simplePerlin3D264*0.5 + 0.5;
				float lerpResult278 = lerp( -1.0 , 1.0 , simplePerlin3D264);
				float lerpResult269 = lerp( 0.0 , lerpResult278 , _GrainAffect);
				#ifdef _GRAINFEATURE_ON
				float staticSwitch282 = lerpResult269;
				#else
				float staticSwitch282 = 0.0;
				#endif
				float grain268 = staticSwitch282;
				float mulTime3_g173 = _TimeParameters.x * _Line1Speed1;
				float2 temp_cast_1 = ((ase_positionWS.y*_Line1Frequency1 + ( mulTime3_g173 + randomoffset385 ))).xx;
				float clampResult42_g173 = clamp( ( ( tex2D( _Line3, temp_cast_1 ).r - _Line1InvertedThickness1 ) * _Line1Hardness1 ) , 0.0 , 1.0 );
				float temp_output_629_0 = clampResult42_g173;
				float temp_output_638_0 = ( temp_output_629_0 * _Line1Alpha1 );
				float4 appendResult683 = (float4(0.0 , 0.0 , 0.0 , temp_output_638_0));
				float mulTime685 = _TimeParameters.x * _Line2Speed;
				float temp_output_688_0 = frac( ( ase_positionWS.y + mulTime685 ) );
				float4 appendResult644 = (float4(( (( maincolor232 * temp_output_629_0 )).xyz * (( maincolor232 * temp_output_688_0 )).xyz ) , ( temp_output_638_0 * ( temp_output_688_0 * _Line2Alpha ) )));
				float4 line_color650 = ( appendResult683 + appendResult644 );
				float C_ZERO596 = 0.0;
				float lerpResult400 = lerp( C_ZERO596 , tex2DNode605.a , _Alpha);
				float alphamask402 = lerpResult400;
				float fresnelalpha579 = clampResult577;
				float clampResult677 = clamp( ( fresnelalpha579 + (line_color650).w ) , 0.0 , 1.0 );
				float4 appendResult679 = (float4(( ColorGlitch689 * ( fresnelcolor580 + grain268 + maincolor232 + (line_color650).xyz ) ) , ( alphamask402 * clampResult677 )));
				

				float Alpha = (appendResult679).w;
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
	
	CustomEditor "AmplifyShaderEditor.MaterialInspector"
	FallBack "Hidden/Shader Graph/FallbackError"
	
	Fallback "Hidden/InternalErrorShader"
}
/*ASEBEGIN
Version=19801
Node;AmplifyShaderEditor.Matrix4X4Node;374;-6064,2336;Inherit;False;InstancedProperty;_CustomMatrix;Custom Matrix;52;0;Create;True;0;0;0;False;0;False;1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1;0;1;FLOAT4x4;0
Node;AmplifyShaderEditor.PosVertexDataNode;381;-5952,2464;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;377;-5600,2272;Inherit;False;2;2;0;FLOAT4x4;0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.BreakToComponentsNode;379;-5440,2144;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.PosVertexDataNode;349;-5568,2000;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WorldPosInputsNode;348;-5584,1760;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.StaticSwitch;351;-5280,1936;Inherit;False;Property;_PositionFeature;Position Feature;52;0;Create;True;0;0;0;False;0;False;0;1;1;True;;KeywordEnum;3;X;Y;Z;Reference;-1;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;378;-5088,2160;Inherit;False;Property;_PositionFeature;Position Feature;22;0;Create;True;0;0;0;False;0;False;0;1;1;True;;KeywordEnum;3;X;Y;Z;Reference;350;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;350;-5264,1808;Inherit;False;Property;_PositionFeature;Position Feature;22;0;Create;True;0;0;0;False;0;False;0;1;1;True;;KeywordEnum;3;X;Y;Z;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;310;1776,-3008;Inherit;False;5020.701;2224.457;Random Glitch;37;493;311;123;312;196;128;122;201;121;197;199;198;192;148;124;191;147;186;129;126;202;127;208;210;211;209;506;505;534;532;527;525;526;531;528;529;530;;1,1,1,1;0;0
Node;AmplifyShaderEditor.StaticSwitch;352;-4656,1840;Inherit;False;Property;_PositionSpaceFeature;Position Space Feature;51;0;Create;True;0;0;0;False;0;False;0;0;0;True;;KeywordEnum;3;World;Local;Custom;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;368;-4352,1920;Inherit;False;Property;_PositionDirection;Position Direction;23;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;384;32,-3728;Inherit;False;Property;_RandomOffset;Random Offset;53;0;Create;True;0;0;0;False;0;False;0;46874.27;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;209;1824,-2048;Inherit;False;1004.303;765.5337;Main Glitch Cycle;11;150;151;149;154;155;152;153;364;393;392;394;;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;367;-4096,1856;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;385;240,-3728;Inherit;False;randomoffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;150;1840,-1520;Inherit;False;Constant;_Glitch1Speed;Glitch 1 Speed;41;0;Create;True;0;0;0;False;0;False;-2.3;-2.3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.VertexToFragmentNode;497;-3872,1936;Inherit;False;False;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;154;2048,-1392;Inherit;False;Constant;_Glitch1SpeedX;Glitch 1 Speed X;43;0;Create;True;0;0;0;False;0;False;-2.05;-2.05;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;149;2032,-1616;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;393;2032,-1520;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;353;-3600,1920;Inherit;False;pos;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;211;3152,-1840;Inherit;False;1719.862;594;Random Glitch;18;365;187;225;158;157;142;156;141;137;139;138;226;366;373;387;389;391;388;;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;364;1920,-1856;Inherit;False;353;pos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;394;2240,-1680;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;151;1920,-1760;Inherit;False;Property;_RandomGlitchTiling;Random Glitch Tiling;42;0;Create;True;0;0;0;False;0;False;2.83;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;155;2288,-1488;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PosFromTransformMatrix;373;3168,-1648;Inherit;False;1;0;FLOAT4x4;1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ScaleAndOffsetNode;152;2272,-1904;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;138;3328,-1360;Inherit;False;Constant;_GlitchPeriodicSpeed;Glitch Periodic Speed;44;0;Create;True;0;0;0;False;0;False;-5.74;-5.74;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;392;2416,-1632;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;141;3456,-1760;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;366;3456,-1648;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;139;3584,-1440;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;156;3808,-1328;Inherit;False;Constant;_GlitchPeriodicSpeedX;Glitch Periodic Speed X;45;0;Create;True;0;0;0;False;0;False;-0.83;-0.83;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;387;3568,-1360;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;153;2672,-1792;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;210;3808,-1136;Inherit;False;764.9996;344.855;Small Glitch lines;5;193;190;194;195;188;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;137;3488,-1520;Inherit;False;Constant;_GlitchPeriodicTiling;Glitch Periodic Tiling;39;0;Create;True;0;0;0;False;0;False;223;223;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;391;3952,-1536;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;157;4032,-1440;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;208;3232,-1040;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.StaticSwitch;365;3600,-1792;Inherit;False;Property;_Keyword2;Keyword 2;51;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;352;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;388;3776,-1456;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;142;3920,-1760;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;190;3856,-928;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.RangedFloatNode;193;3856,-1024;Inherit;False;Constant;_Float2;Float 2;34;0;Create;True;0;0;0;False;0;False;20;20.35;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;389;4192,-1536;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;194;4048,-1088;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;158;4192,-1728;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;195;4192,-1056;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;187;4368,-1696;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;225;4256,-1440;Inherit;False;Property;_RandomGlitchConstant;Random Glitch Constant;40;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;202;4384,-2192;Inherit;False;Constant;_GlitchRemapMinNew;Glitch Remap Min New;24;0;Create;True;0;0;0;False;0;False;-1;-1;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;226;4704,-1584;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;126;4384,-2416;Inherit;False;Constant;_GlitchRemapMinOld;Glitch Remap Min Old;24;0;Create;True;0;0;0;False;0;False;0;0;-2;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;188;4336,-1056;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;129;4368,-2096;Inherit;False;Constant;_GlitchRemapMaxNew;Glitch Remap Max New;26;0;Create;True;0;0;0;False;0;False;1;1;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;127;4384,-2304;Inherit;False;Constant;_GlitchRemapMaxOld;Glitch Remap Max Old;25;0;Create;True;0;0;0;False;0;False;1;1;-2;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;147;5168,-2160;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;186;4880,-2752;Inherit;True;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;191;5152,-1920;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;610;-736,-3264;Inherit;False;1923.354;1230.809;Lines;8;650;684;683;644;639;640;611;612;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;85;-2089.168,1936.791;Inherit;False;1829.211;723.5701;Glitch Line;16;81;94;87;90;86;89;84;88;307;308;363;479;491;507;508;537;;1,1,1,1;0;0
Node;AmplifyShaderEditor.ClampOpNode;148;5424,-2096;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;198;5520,-1600;Inherit;False;Constant;_Float3;Float 3;35;0;Create;True;0;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;192;5392,-1824;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;124;5168,-2352;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;562;-720,-5280;Inherit;False;1521.172;436.0327;Normal Map;9;594;591;590;589;588;587;586;585;584;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;549;-720,-4032;Inherit;False;670.612;448.8643;Color;4;232;604;605;231;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;612;-704,-2400;Inherit;False;1044.256;339.4957;Line 2;10;685;687;613;686;633;636;688;635;631;627;;1,1,1,1;0;0
Node;AmplifyShaderEditor.LerpOp;199;5632,-1872;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;121;4880,-2960;Inherit;False;Property;_RandomGlitchOffset;Random Glitch Offset;26;0;Create;True;0;0;0;False;0;False;-0.5,0,0;1,1,1;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;197;5584,-2320;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;84;-1704.07,1985.222;Inherit;False;Property;_LineGlitchOffset;Line Glitch Offset;25;0;Create;True;0;0;0;False;0;False;0.03,0,0;0.03,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;585;-656,-5184;Inherit;False;Property;_NormalScale;NormalScale;4;0;Create;True;0;0;0;False;0;False;0;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;605;-704,-3792;Inherit;True;Property;_MainTexture;Main Texture;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.ColorNode;231;-688,-3984;Inherit;False;Property;_MainColor;Main Color;0;1;[HDR];Create;True;0;0;0;False;0;False;0.620945,1.420074,3.953349,0.05098039;1.216786,0.3015485,0.1974888,0.05098039;True;True;0;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.RangedFloatNode;613;-688,-2176;Inherit;False;Property;_Line2Speed;Line 2 Speed;16;0;Create;True;0;0;0;False;0;False;-1;-1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectScaleNode;505;5200,-2704;Inherit;False;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;201;5728,-2176;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TransformDirectionNode;122;5152,-2880;Inherit;False;View;Object;False;Fast;False;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.ObjectScaleNode;507;-1488.19,2206.576;Inherit;False;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;479;-1964.807,2162.083;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TransformDirectionNode;94;-1500.492,2031.106;Inherit;False;View;Object;False;Fast;False;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;87;-1985.12,2442.207;Inherit;False;Property;_LineGlitchFrequency;Line Glitch Frequency;29;0;Create;True;0;0;0;False;0;False;0.2;0.2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;90;-1987.701,2358.637;Inherit;False;Property;_LineGlitchHardness;Line Glitch Hardness;30;0;Create;True;0;0;0;False;0;False;5;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;363;-1866.639,2236.446;Inherit;False;353;pos;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;86;-1985.9,2516.262;Inherit;False;Property;_LineGlitchInvertedThickness;Line Glitch Inverted Thickness;31;0;Create;True;0;0;0;False;0;False;0.825;0.801;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;88;-2067.168,2296.9;Inherit;False;Property;_LineGlitchSpeed;Line Glitch Speed;28;0;Create;True;0;0;0;False;0;False;-0.26;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;89;-1984,1968;Inherit;True;Property;_LineGlitch;Line Glitch;24;2;[Header];[NoScaleOffset];Create;True;1;Glitch;0;0;False;0;False;88eb97e78f86c604bb00864c0dbeffc1;a6ddbfbf28b5acb44921ca832c5c7f18;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.CommentaryNode;611;-720,-3216;Inherit;False;1049.833;800.9198;Line 1;13;619;691;620;617;623;637;638;632;634;630;629;621;625;;1,1,1,1;0;0
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;586;-352,-5040;Inherit;False;Tangent;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SamplerNode;584;-464,-5232;Inherit;True;Property;_NormalMap;NormalMap;3;0;Create;True;0;0;0;False;0;False;-1;None;d5ecf639375fb7641b0c39b0025bbf1f;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;6;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;604;-416,-3856;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleTimeNode;685;-512,-2176;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldPosInputsNode;687;-512,-2336;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;128;5520,-2704;Inherit;False;Property;_RandomGlitchAmount;Random Glitch Amount;27;0;Create;True;0;0;0;False;0;False;0.089;0.05;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;196;5808,-2400;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;506;5472,-2864;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;508;-1248.843,2089.367;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;537;-1532.734,2384.947;Inherit;False;Hologram Line;-1;;175;a6b4840f4c8a45041b49734edbb63562;0;7;37;SAMPLER2D;0;False;44;FLOAT;0;False;43;FLOAT;0;False;13;FLOAT;1;False;14;FLOAT;1;False;15;FLOAT;2;False;16;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;694;-753.0891,-6354;Inherit;False;2211.089;540.0215;Color Glitcht;18;689;181;175;184;183;174;176;168;166;396;395;167;383;163;386;164;165;160;;1,1,1,1;0;0
Node;AmplifyShaderEditor.DotProductOpNode;587;-160,-5152;Inherit;True;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;232;-256,-3856;Inherit;False;maincolor;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;686;-320,-2256;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;625;-624,-2960;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;621;-624,-3152;Inherit;True;Property;_Line3;Line 1;10;2;[Header];[NoScaleOffset];Create;True;2;Lines;_;0;0;False;0;False;88eb97e78f86c604bb00864c0dbeffc1;ef301d4822ac111469bda406e1b4bc7c;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;623;-688,-2496;Inherit;False;Property;_Line1InvertedThickness1;Line 1 Inverted Thickness;14;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;617;-624,-2576;Inherit;False;Property;_Line1Frequency1;Line 1 Frequency;12;0;Create;True;0;0;0;False;0;False;100;101;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;620;-592,-2656;Inherit;False;Property;_Line1Hardness1;Line 1 Hardness;13;0;Create;True;0;0;0;False;0;False;1.45;1.45;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.WorldPosInputsNode;691;-592,-2880;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;619;-592,-2736;Inherit;False;Property;_Line1Speed1;Line 1 Speed;11;0;Create;True;0;0;0;False;0;False;-3.57;-3.57;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;312;6032,-2912;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;123;5952,-2832;Inherit;False;3;3;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;81;-1091.057,2172.398;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;308;-1093.97,2020.648;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;588;64,-5152;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;-1;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;589;-16,-4928;Inherit;False;Property;_NormalAffect;NormalAffect;5;0;Create;True;0;0;0;False;0;False;0;0.589;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;629;-352,-2960;Inherit;False;Hologram Line;-1;;173;a6b4840f4c8a45041b49734edbb63562;0;7;37;SAMPLER2D;0;False;44;FLOAT;0;False;43;FLOAT;0;False;13;FLOAT;1;False;14;FLOAT;1;False;15;FLOAT;2;False;16;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;630;-224,-3040;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;627;-256,-2336;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FractNode;688;-192,-2256;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;160;-736,-6128;Inherit;False;Constant;_ColorPeriodicSpeed;Color Periodic Speed;46;0;Create;True;0;0;0;False;0;False;-15;-15;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;311;6224,-2832;Inherit;False;Property;_RandomGlitchFeature;Random Glitch Feature;45;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;307;-898.1913,2090.678;Inherit;False;Property;_LineGlitchFeature;Line Glitch Feature;44;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;281;-720,-5776;Inherit;False;1563.049;472.3077;Grain;11;270;273;269;282;268;265;602;272;264;278;283;;1,1,1,1;0;0
Node;AmplifyShaderEditor.LerpOp;590;272,-5072;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;634;-16,-3040;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;632;-224,-2736;Inherit;False;Property;_Line1Alpha1;Line 1 Alpha;15;0;Create;True;0;0;0;False;0;False;0.15;0.15;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;631;-48,-2336;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;633;-240,-2160;Inherit;False;Property;_Line2Alpha;Line 2 Alpha;17;0;Create;True;0;0;0;False;0;False;0.1;0.1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;165;-512,-6128;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;164;-576,-5952;Inherit;False;Constant;_ColorPeriodicSpeedX;Color Periodic Speed X;45;0;Create;True;0;0;0;False;0;False;-0.5;-0.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;386;-544,-6208;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;493;6560,-2800;Inherit;False;randomglitch;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;491;-555.8857,2103.779;Inherit;False;lineglitch;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;563;-720,-4368;Inherit;False;1104.62;310.3358;Fresnel Opacity;7;579;577;575;565;573;569;566;;1,1,1,1;0;0
Node;AmplifyShaderEditor.OneMinusNode;591;432,-5072;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;638;-16,-2944;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;637;128,-3040;Inherit;False;True;True;True;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;635;112,-2336;Inherit;False;True;True;True;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;636;-48,-2224;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;273;-704,-5392;Inherit;False;1;0;FLOAT;10;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldPosInputsNode;265;-704,-5664;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.IntNode;602;-704,-5504;Inherit;False;Property;_GrainScale;Grain Scale;20;0;Create;True;0;0;0;False;0;False;1000;0;False;0;1;INT;0
Node;AmplifyShaderEditor.RangedFloatNode;163;-336,-6288;Inherit;False;Constant;_ColorPeriodcTiling;Color Periodc Tiling;38;0;Create;True;0;0;0;False;0;False;223;223;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;383;-240,-6208;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;167;-304,-5952;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;395;-336,-6032;Inherit;False;385;randomoffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;492;2864,-416;Inherit;False;491;lineglitch;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;494;2864,-336;Inherit;False;493;randomglitch;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;564;-720,-4816;Inherit;False;1093.118;409.6844;Fresnel;8;580;578;582;574;583;567;568;571;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;566;-672,-4192;Inherit;False;Property;_FresnelAlphaPower;Fresnel Alpha Power;9;0;Create;True;0;0;0;False;0;False;2;3.65;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;565;-672,-4272;Inherit;False;Property;_FresnelAlphaScale;Fresnel Alpha Scale;8;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;594;592,-5072;Inherit;False;NormalAffect;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;272;-480,-5568;Inherit;False;3;0;FLOAT3;1000,0,0;False;1;FLOAT3;1,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;640;416,-2752;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;639;416,-2640;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;396;-64,-6000;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;166;-96,-6304;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;159;3104,-400;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FresnelNode;569;-432,-4320;Inherit;False;Standard;WorldNormal;ViewDir;False;True;5;0;FLOAT3;0,0,1;False;4;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;5;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;573;-432,-4144;Inherit;False;594;NormalAffect;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;567;-688,-4624;Inherit;False;Property;_FresnelPower;Fresnel Power;7;0;Create;True;0;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;568;-688,-4704;Inherit;False;Property;_FresnelScale;Fresnel Scale;6;1;[Header];Create;True;2;FRESNEL;_;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;264;-272,-5568;Inherit;False;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;644;592,-2704;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;683;592,-2864;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;168;160,-6128;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;410;3232,-400;Inherit;False;vertexoffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;595;32,-3904;Inherit;False;Constant;_ConstantZero;Constant Zero;47;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;575;-192,-4256;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FresnelNode;571;-448,-4752;Inherit;False;Standard;WorldNormal;ViewDir;False;True;5;0;FLOAT3;0,0,1;False;4;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;5;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;583;-448,-4576;Inherit;False;594;NormalAffect;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;278;-32,-5600;Inherit;False;3;0;FLOAT;-1;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;684;752,-2704;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;270;-160,-5456;Inherit;False;Property;_GrainAffect;Grain Affect;19;1;[Header];Create;True;2;Grain;_;0;0;False;0;False;1;0.271;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;176;320,-6128;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;510;3600,-400;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;509;3600,-176;Inherit;False;410;vertexoffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;596;240,-3904;Inherit;False;C_ZERO;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;522;-720,-3552;Inherit;False;727.6426;243.0996;Alpha ;4;402;400;609;655;;1,1,1,1;0;0
Node;AmplifyShaderEditor.ClampOpNode;577;-48,-4256;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;574;-192,-4688;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;582;-416,-4496;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;269;192,-5520;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;283;192,-5616;Inherit;False;Constant;_Float4;Float 4;46;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;650;880,-2704;Inherit;False;line_color;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.TFHCRemapNode;174;608,-6128;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;-0.61;False;4;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;511;3776,-80;Inherit;False;Property;_Voxelization;Voxelization;56;0;Create;True;0;0;0;False;0;False;100;100;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;512;3872,-352;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;579;144,-4256;Inherit;False;fresnelalpha;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;609;-608,-3504;Inherit;False;596;C_ZERO;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;655;-688,-3408;Inherit;False;Property;_Alpha;Alpha;2;0;Create;True;0;0;0;False;0;False;0;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;578;-48,-4592;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;654;1520,-3632;Inherit;False;650;line_color;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.StaticSwitch;282;352,-5536;Inherit;False;Property;_GrainFeature;Grain Feature;18;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;183;832,-6224;Inherit;False;Constant;_Float1;Float 1;47;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;184;704,-5920;Inherit;False;Property;_ColorGlitchAffect;Color Glitch Affect;21;0;Create;True;0;0;0;False;0;False;0.5;0.153;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;175;848,-6128;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;513;4032,-256;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;400;-400,-3488;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;653;1760,-3520;Inherit;False;579;fresnelalpha;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;657;1728,-3440;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;580;128,-4592;Inherit;False;fresnelcolor;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;268;608,-5536;Inherit;False;grain;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;181;1056,-6128;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RoundOpNode;514;4224,-272;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;402;-224,-3488;Inherit;False;alphamask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;676;1968,-3488;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;601;1840,-3776;Inherit;False;268;grain;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;603;1840,-3712;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;437;1840,-3856;Inherit;False;580;fresnelcolor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;656;1808,-3632;Inherit;False;True;True;True;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;689;1216,-6128;Inherit;False;ColorGlitch;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;516;4272,96;Inherit;False;Property;_VoxelizationAffect;Voxelization Affect;57;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;515;4400,-272;Inherit;True;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ClampOpNode;677;2112,-3552;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;599;2064,-3808;Inherit;False;4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;690;1840,-3936;Inherit;False;689;ColorGlitch;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;406;2080,-3632;Inherit;False;402;alphamask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;517;4720,-320;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;652;2272,-3680;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;185;2208,-3936;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;518;4912,-384;Inherit;False;Property;_VoxelizationFeature;Voxelization Feature;58;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;679;2416,-3808;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.CommentaryNode;322;-6309.657,5392.202;Inherit;False;2289.167;729.9302;Soft Intersection;21;239;238;247;248;242;244;313;314;315;261;316;321;245;344;409;412;411;461;465;467;469;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;413;-6233.782,6283.463;Inherit;False;2399.167;724.9302;Soft Intersection;21;430;431;463;428;426;429;424;425;423;421;460;422;420;418;419;417;416;427;414;415;471;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;323;-2176.098,5795.262;Inherit;False;1985.258;718.9717;Mask;15;254;256;249;250;253;258;252;251;257;339;340;499;500;501;502;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;227;-1961.202,1464.31;Inherit;False;1755;439.4824;Dissolve Hide;10;432;337;338;218;215;214;212;221;362;220;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;519;5264,-384;Inherit;False;ModifiedVertexPosition;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;597;32,-3824;Inherit;False;Constant;_ConstantOne;Constant One;47;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;598;240,-3824;Inherit;False;C_ONE;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;412;-6290.911,5878.647;Inherit;False;410;vertexoffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PosVertexDataNode;409;-6283.292,5682.31;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;414;-6215.036,6769.908;Inherit;False;410;vertexoffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PosVertexDataNode;415;-6207.417,6573.571;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;239;-6259.657,6007.132;Inherit;False;Property;_SoftIntersection1Distance;Soft Intersection 1 Distance;33;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;416;-5996.036,6723.908;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;411;-6071.911,5832.647;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;427;-6183.782,6898.393;Inherit;False;Property;_SoftIntersection2Distance;Soft Intersection 2 Distance;32;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.DepthFade;417;-5862.251,6787.997;Inherit;False;True;True;True;2;1;FLOAT3;0,0,0;False;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.DepthFade;238;-5938.126,5896.736;Inherit;False;True;True;True;2;1;FLOAT3;0,0,0;False;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;463;-5308.75,6923.549;Inherit;False;Property;_SoftIntersection2Affect;Soft Intersection 2 Affect;36;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;242;-5640.949,5888.244;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;419;-5748.796,6443.817;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;248;-5776.702,6009.678;Inherit;False;Property;_SoftIntersection1Intensity;Soft Intersection 1 Intensity;37;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;461;-5433.161,6039.013;Inherit;False;Property;_SoftIntersection1Affect;Soft Intersection 1 Affect;38;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;418;-5579.074,6789.505;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;247;-5824.671,5552.556;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;428;-5630.155,6897.453;Inherit;False;Property;_SoftIntersection2Intensity;Soft Intersection 2 Intensity;35;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;244;-5424.438,5742.134;Inherit;False;4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PowerNode;344;-5351.301,5586.396;Inherit;False;True;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;420;-5349.563,6595.395;Inherit;False;4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PowerNode;422;-5210.426,6492.657;Inherit;False;True;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;467;-5176.697,5610.676;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;421;-5187.16,6635.463;Inherit;False;True;True;True;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;313;-5263.035,5744.202;Inherit;False;True;True;True;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;460;-5194.136,6763.219;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;469;-5319.677,5888.282;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;471;-5070.44,6499.274;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;423;-5178.159,6333.463;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TransformPositionNode;257;-2114.098,6266.84;Inherit;False;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;316;-5254.034,5442.202;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;314;-5033.034,5786.202;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.Vector3Node;251;-2090.248,5993.262;Inherit;False;Property;_MaskCenter;Mask Center;34;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.DynamicAppendNode;424;-4927.159,6425.463;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;321;-5003.034,5534.202;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;425;-4931.159,6671.463;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.StaticSwitch;315;-4816.035,5658.202;Inherit;False;Property;_SoftIntersection1Feature;Soft Intersection 1 Feature;47;0;Create;True;0;0;0;False;0;False;0;0;0;True;;KeywordEnum;3;Off;Alpha;Color;Create;False;True;All;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleAddOpNode;258;-1831.098,6097.84;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;429;-4716.16,6550.463;Inherit;False;Property;_SoftIntersection2Feature;Soft Intersection 2 Feature;46;0;Create;True;0;0;0;False;0;False;0;0;0;True;;KeywordEnum;3;Off;Alpha;Color;Create;False;True;All;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ComponentMaskNode;426;-4289.16,6726.463;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;499;-1705.199,6001.581;Inherit;False;Property;_MaskLocalFeature;Mask Local Feature;54;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;362;-1921.947,1543.32;Inherit;False;357;vPos;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;465;-4479.439,5797.266;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;253;-1499.248,6329.262;Inherit;False;Property;_MaskFalloff;Mask Falloff;43;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;252;-1729.248,6332.262;Inherit;False;Property;_MaskSize;Mask Size;39;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.WorldPosInputsNode;250;-2119.248,5828.262;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.Vector3Node;220;-1926.789,1679.506;Inherit;False;Property;_DissolveScale;Dissolve Scale;41;0;Create;True;0;0;0;False;0;False;0.1,1.01,5;15,0.1,0.1;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;431;-4089.615,6725.208;Inherit;False;intersectionalpha2;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;249;-1377.45,5929.36;Inherit;False;BoxMask;-1;;176;9dce4093ad5a42b4aa255f0153c4f209;0;4;1;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;10;FLOAT3;0,0,0;False;17;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;221;-1683.789,1590.506;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;261;-4247.49,5836.947;Inherit;False;intersectionalpha1;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;214;-1749.202,1789.792;Inherit;False;Property;_DissolveHide;Dissolve Hide;48;0;Create;True;0;0;0;False;0;False;-1;-1;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;212;-1461.372,1514.31;Inherit;True;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;256;-1051.061,5957.033;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;500;-897.7198,6146.505;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;502;-965.7198,6268.505;Inherit;False;Property;_MaskInversion;Mask Inversion;55;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;215;-1137.202,1610.792;Inherit;True;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;338;-925.9092,1536.336;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;218;-909.2017,1625.792;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;340;-1035.714,5843.033;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;501;-699.7198,6139.505;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;339;-674.7141,5962.033;Inherit;False;Property;_MaskFeature;Mask Feature;50;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;337;-716.3076,1614.569;Inherit;False;Property;_DissolveFeature;Dissolve Feature;49;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;254;-419.5153,5950.769;Inherit;False;mask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;432;-435.5378,1646.387;Inherit;False;dissolve;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;245;-4244.452,5732.669;Inherit;False;intersection1;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;430;-4106.577,6592.93;Inherit;False;intersection2;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;451;-904.1172,335.3077;Inherit;False;431;intersectionalpha2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;263;-1076.079,-160.5902;Inherit;False;261;intersectionalpha1;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;457;-573.1609,247.4301;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;452;-807.0956,583.8937;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;450;-590.0953,327.8937;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;449;-545.9037,462.2688;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;441;-719.1655,-32.32913;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;332;-763.3571,-166.7042;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;335;-980.3573,89.29573;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;438;-724.0007,-535.3825;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.WireNode;439;-736.1655,-228.3291;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;299;-944,-864;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;455;-560.9036,502.3688;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;453;-539.9037,705.2688;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;458;-724.1306,180.5571;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;454;-573.9037,575.2689;Inherit;False;431;intersectionalpha2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;447;-343.0953,286.8937;Inherit;False;Property;_Keyword0;Keyword 0;42;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;429;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;330;-516.3571,-207.7042;Inherit;False;Property;_Keyword0;Keyword 0;43;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;315;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;444;-734.1654,7.770939;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;443;-747.1655,80.67097;Inherit;False;261;intersectionalpha1;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;301;-748.1158,-354.381;Inherit;False;False;False;False;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;236;-453.6711,-528.7764;Inherit;False;False;False;False;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;435;-496,-368;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;448;-253.0953,527.8937;Inherit;False;Property;_Keyword0;Keyword 0;42;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;429;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;58;-6.768324,-573.8707;Inherit;False;5;5;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;334;-426.3571,33.29572;Inherit;False;Property;_Keyword0;Keyword 0;43;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;315;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;333;133.6427,-473.7041;Inherit;False;3;3;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;255;460.0316,-418.8165;Inherit;False;254;mask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;62;302.3969,-575.8758;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;1,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;433;456.5834,-493.5936;Inherit;False;432;dissolve;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;246;-484.2697,-789.547;Inherit;False;245;intersection1;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;456;-478.5144,-626.7694;Inherit;False;430;intersection2;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;271;-467.4747,-710.1898;Inherit;False;268;grain;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;300;-488.9832,-867.1477;Inherit;False;True;True;True;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;235;-496,-1040;Inherit;False;232;maincolor;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;224;784,-368;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;504;-1056,-1328;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.VertexColorNode;503;-1280,-1296;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ComponentMaskNode;533;-896,-1312;Inherit;False;False;False;False;True;1;0;COLOR;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;380;-4928,2416;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;375;-4640,2000;Inherit;False;Property;_PositionSpaceFeature;Position Space Feature;48;0;Create;True;0;0;0;False;0;False;0;0;0;True;;KeywordEnum;2;World;Local;Reference;352;False;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;357;-3824,2096;Inherit;False;vPos;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;680;2576,-3728;Inherit;False;False;False;False;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;695;2528,-3600;Inherit;False;519;ModifiedVertexPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;524;2800,-3808;Float;False;True;-1;2;AmplifyShaderEditor.MaterialInspector;0;13;Hologram Shader IA;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;9;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;_CullMode;False;False;False;False;False;False;False;False;True;True;True;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;UniversalMaterialType=Unlit;True;5;True;6;d3d11;glcore;gles;gles3;metal;vulkan;0;False;True;1;5;False;;10;False;;1;1;False;;10;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;True;True;1;False;_Alpha;True;3;False;;True;False;0;False;;0;False;;True;1;LightMode=UniversalForward;False;False;0;Hidden/InternalErrorShader;0;0;Standard;27;Surface;1;637972967283466847;  Blend;0;638751246854332985;Two Sided;0;638751281489639942;Alpha Clipping;0;638701363033569914;  Use Shadow Threshold;0;0;Forward Only;0;638751281839009918;Cast Shadows;0;637972967296698888;Receive Shadows;0;637972967305116547;Motion Vectors;1;0;  Add Precomputed Velocity;0;638750438578206950;GPU Instancing;1;0;LOD CrossFade;0;0;Built-in Fog;0;0;Meta Pass;0;638751247411743498;Extra Pre Pass;0;638751247367536031;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Write Depth;0;638751281890919512;  Early Z;1;638751249679503756;Vertex Position,InvertActionOnDeselection;0;637972966021148095;0;11;False;True;False;False;False;False;False;False;False;False;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;523;1968,-3984;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;530;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;529;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;528;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;5;False;;10;False;;1;1;False;;10;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;True;1;True;_ZWrite;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;531;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormals;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;526;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;525;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;527;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;532;2186.873,-698.6207;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;534;2186.873,-598.6207;Float;False;False;-1;3;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;MotionVectors;0;10;MotionVectors;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;False;False;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=MotionVectors;False;False;0;;0;0;Standard;0;False;0
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
WireConnection;385;0;384;0
WireConnection;497;0;367;0
WireConnection;149;0;150;0
WireConnection;353;0;497;0
WireConnection;394;0;149;0
WireConnection;394;1;393;0
WireConnection;155;0;154;0
WireConnection;152;0;364;0
WireConnection;152;1;151;0
WireConnection;152;2;394;0
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
WireConnection;201;0;197;0
WireConnection;201;1;199;0
WireConnection;122;0;121;0
WireConnection;94;0;84;0
WireConnection;584;5;585;0
WireConnection;604;0;231;5
WireConnection;604;1;605;5
WireConnection;685;0;613;0
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
WireConnection;587;0;584;0
WireConnection;587;1;586;0
WireConnection;232;0;604;0
WireConnection;686;0;687;2
WireConnection;686;1;685;0
WireConnection;123;0;506;0
WireConnection;123;1;196;0
WireConnection;123;2;128;0
WireConnection;81;0;508;0
WireConnection;81;1;537;0
WireConnection;588;0;587;0
WireConnection;629;37;621;0
WireConnection;629;44;625;0
WireConnection;629;43;691;2
WireConnection;629;13;619;0
WireConnection;629;14;620;0
WireConnection;629;15;617;0
WireConnection;629;16;623;0
WireConnection;688;0;686;0
WireConnection;311;1;312;0
WireConnection;311;0;123;0
WireConnection;307;1;308;0
WireConnection;307;0;81;0
WireConnection;590;1;588;0
WireConnection;590;2;589;0
WireConnection;634;0;630;0
WireConnection;634;1;629;0
WireConnection;631;0;627;0
WireConnection;631;1;688;0
WireConnection;165;0;160;0
WireConnection;493;0;311;0
WireConnection;491;0;307;0
WireConnection;591;0;590;0
WireConnection;638;0;629;0
WireConnection;638;1;632;0
WireConnection;637;0;634;0
WireConnection;635;0;631;0
WireConnection;636;0;688;0
WireConnection;636;1;633;0
WireConnection;383;0;386;0
WireConnection;383;1;165;0
WireConnection;167;0;164;0
WireConnection;594;0;591;0
WireConnection;272;0;265;0
WireConnection;272;1;602;0
WireConnection;272;2;273;0
WireConnection;640;0;637;0
WireConnection;640;1;635;0
WireConnection;639;0;638;0
WireConnection;639;1;636;0
WireConnection;396;0;395;0
WireConnection;396;1;167;0
WireConnection;166;1;163;0
WireConnection;166;2;383;0
WireConnection;159;0;492;0
WireConnection;159;1;494;0
WireConnection;569;2;565;0
WireConnection;569;3;566;0
WireConnection;264;0;272;0
WireConnection;644;0;640;0
WireConnection;644;3;639;0
WireConnection;683;3;638;0
WireConnection;168;0;166;0
WireConnection;168;1;396;0
WireConnection;410;0;159;0
WireConnection;575;0;569;0
WireConnection;575;1;573;0
WireConnection;571;2;568;0
WireConnection;571;3;567;0
WireConnection;278;2;264;0
WireConnection;684;0;683;0
WireConnection;684;1;644;0
WireConnection;176;0;168;0
WireConnection;596;0;595;0
WireConnection;577;0;575;0
WireConnection;574;0;571;0
WireConnection;574;1;583;0
WireConnection;269;1;278;0
WireConnection;269;2;270;0
WireConnection;650;0;684;0
WireConnection;174;0;176;0
WireConnection;512;0;510;0
WireConnection;512;1;509;0
WireConnection;579;0;577;0
WireConnection;578;0;574;0
WireConnection;578;1;582;0
WireConnection;578;2;577;0
WireConnection;282;1;283;0
WireConnection;282;0;269;0
WireConnection;175;0;174;0
WireConnection;513;0;512;0
WireConnection;513;1;511;0
WireConnection;400;0;609;0
WireConnection;400;1;605;4
WireConnection;400;2;655;0
WireConnection;657;0;654;0
WireConnection;580;0;578;0
WireConnection;268;0;282;0
WireConnection;181;0;183;0
WireConnection;181;1;175;0
WireConnection;181;2;184;0
WireConnection;514;0;513;0
WireConnection;402;0;400;0
WireConnection;676;0;653;0
WireConnection;676;1;657;0
WireConnection;656;0;654;0
WireConnection;689;0;181;0
WireConnection;515;0;514;0
WireConnection;515;1;511;0
WireConnection;677;0;676;0
WireConnection;599;0;437;0
WireConnection;599;1;601;0
WireConnection;599;2;603;0
WireConnection;599;3;656;0
WireConnection;517;0;512;0
WireConnection;517;1;515;0
WireConnection;517;2;516;0
WireConnection;652;0;406;0
WireConnection;652;1;677;0
WireConnection;185;0;690;0
WireConnection;185;1;599;0
WireConnection;518;1;512;0
WireConnection;518;0;517;0
WireConnection;679;0;185;0
WireConnection;679;3;652;0
WireConnection;519;0;518;0
WireConnection;598;0;597;0
WireConnection;416;0;415;0
WireConnection;416;1;414;0
WireConnection;411;0;409;0
WireConnection;411;1;412;0
WireConnection;417;1;416;0
WireConnection;417;0;427;0
WireConnection;238;1;411;0
WireConnection;238;0;239;0
WireConnection;242;0;238;0
WireConnection;418;0;417;0
WireConnection;244;0;247;0
WireConnection;244;1;242;0
WireConnection;244;2;248;0
WireConnection;244;3;461;0
WireConnection;344;0;238;0
WireConnection;344;1;248;0
WireConnection;420;0;419;0
WireConnection;420;1;418;0
WireConnection;420;2;428;0
WireConnection;420;3;463;0
WireConnection;422;0;417;0
WireConnection;422;1;428;0
WireConnection;467;1;344;0
WireConnection;467;2;461;0
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
WireConnection;314;0;313;0
WireConnection;314;3;469;0
WireConnection;424;3;471;0
WireConnection;321;3;467;0
WireConnection;425;0;421;0
WireConnection;425;3;460;0
WireConnection;315;1;316;0
WireConnection;315;0;321;0
WireConnection;315;2;314;0
WireConnection;258;0;251;0
WireConnection;258;1;257;0
WireConnection;429;1;423;0
WireConnection;429;0;424;0
WireConnection;429;2;425;0
WireConnection;426;0;429;0
WireConnection;499;1;251;0
WireConnection;499;0;258;0
WireConnection;465;0;315;0
WireConnection;431;0;426;0
WireConnection;249;1;250;0
WireConnection;249;4;499;0
WireConnection;249;10;252;0
WireConnection;249;17;253;0
WireConnection;221;0;362;0
WireConnection;221;1;220;0
WireConnection;261;0;465;0
WireConnection;212;0;221;0
WireConnection;256;0;249;0
WireConnection;500;0;256;0
WireConnection;215;0;212;0
WireConnection;215;1;214;0
WireConnection;218;0;215;0
WireConnection;501;0;256;0
WireConnection;501;1;500;0
WireConnection;501;2;502;0
WireConnection;339;1;340;0
WireConnection;339;0;501;0
WireConnection;337;1;338;0
WireConnection;337;0;218;0
WireConnection;254;0;339;0
WireConnection;432;0;337;0
WireConnection;245;0;315;0
WireConnection;430;0;429;0
WireConnection;457;0;451;0
WireConnection;449;0;451;0
WireConnection;441;0;263;0
WireConnection;439;0;263;0
WireConnection;455;0;452;0
WireConnection;453;0;452;0
WireConnection;458;0;335;0
WireConnection;447;1;457;0
WireConnection;447;0;450;0
WireConnection;447;2;449;0
WireConnection;330;1;439;0
WireConnection;330;0;332;0
WireConnection;330;2;441;0
WireConnection;444;0;335;0
WireConnection;301;0;299;0
WireConnection;236;0;438;0
WireConnection;448;1;455;0
WireConnection;448;0;454;0
WireConnection;448;2;453;0
WireConnection;58;0;236;0
WireConnection;58;1;435;0
WireConnection;58;2;301;0
WireConnection;58;3;330;0
WireConnection;58;4;447;0
WireConnection;334;1;444;0
WireConnection;334;0;443;0
WireConnection;334;2;458;0
WireConnection;333;0;58;0
WireConnection;333;1;334;0
WireConnection;333;2;448;0
WireConnection;62;0;333;0
WireConnection;300;0;299;0
WireConnection;504;1;503;0
WireConnection;533;0;504;0
WireConnection;380;0;377;0
WireConnection;375;1;348;0
WireConnection;375;0;349;0
WireConnection;375;2;380;0
WireConnection;357;0;375;0
WireConnection;680;0;679;0
WireConnection;524;2;679;0
WireConnection;524;3;680;0
WireConnection;524;5;695;0
ASEEND*/
//CHKSM=6B8CB9380EDB50C3CE248E51BCC64D035D84E9F1