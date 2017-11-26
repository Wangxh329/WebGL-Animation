let core_dependencies = "Vec Mat Mat4 Shape Keyboard_Manager Graphics_State Light Color Graphics_Addresses Shader Canvas_Manager Texture Scene_Component Object_From_File Code_Manager".split(' ');
let all_dependencies =  "Triangle Square Tetrahedron Windmill Subdivision_Sphere Cube Phong_Model Funny_Shader Movement_Controls Global_Info_Table Grid_Patch Surface_Of_Revolution Regular_2D_Polygon Cylindrical_Tube Cone_Tip Torus Grid_Sphere Closed_Cone Rounded_Closed_Cone Capped_Cylinder Rounded_Capped_Cylinder Axis_Arrows Fake_Bump_Map".split(' ');

  // *********** TRIANGLE ***********
class Triangle extends Shape    // First, the simplest possible Shape – one triangle.  It has 3 vertices, each
{ constructor()                 // having their own 3D position, normal vector, and texture-space coordinate.
    { super();
      this.positions      = [ Vec.of(0,0,0), Vec.of(1,0,0), Vec.of(0,1,0) ];   // Specify the 3 vertices -- the point cloud that our Triangle needs.
      this.normals        = [ Vec.of(0,0,1), Vec.of(0,0,1), Vec.of(0,0,1) ];   // ...
      this.texture_coords = [ Vec.of(0,0),   Vec.of(1,0),   Vec.of(0,1)   ];   // ...
      this.indices        = [ 0, 1, 2 ];                                       // Index into our vertices to connect them into a whole Triangle.
    }
}       

  // *********** SQUARE ***********
class Square extends Shape      // A square, demonstrating shared vertices.  On any planar surface, the interior edges don't make any important seams.
{ constructor()                 // In these cases there's no reason not to re-use data of the common vertices between triangles.  This makes all the
    { super();                  // vertex arrays (position, normals, etc) smaller and more cache friendly.
      this.positions     .push( ...Vec.cast( [-1,-1,0], [1,-1,0], [-1,1,0], [1,1,0] ) );     // Specify the 4 vertices -- the point cloud that our Square needs.
      this.normals       .push( ...Vec.cast( [0,0,1],   [0,0,1],  [0,0,1],  [0,0,1] ) );     // ...
      this.texture_coords.push( ...Vec.cast( [0,0],     [1,0],    [0,1],    [1,1]   ) );     // ...
      this.indices       .push( 0, 1, 2,     1, 3, 2 );                                      // Two triangles this time, indexing into four distinct vertices.
    }
}

  // *********** TETRAHEDRON ***********
class Tetrahedron extends Shape            // A demo of flat vs smooth shading (a boolean argument selects which one). Also our first 3D, non-planar shape.
{ constructor( using_flat_shading ) 
    { super();
      var a = 1/Math.sqrt(3);
      if( !using_flat_shading )                                         // Method 1:  A tetrahedron with shared vertices.  Compact, performs better,
      {                                                                 // but can't produce flat shading or discontinuous seams in textures.
          this.positions     .push( ...Vec.cast( [ 0, 0, 0], [1,0,0], [0,1,0], [0,0,1] ) );          
          this.normals       .push( ...Vec.cast( [-a,-a,-a], [1,0,0], [0,1,0], [0,0,1] ) );          
          this.texture_coords.push( ...Vec.cast( [ 0, 0   ], [1,0  ], [0,1, ], [1,1  ] ) );
          this.indices       .push( 0, 1, 2,   0, 1, 3,   0, 2, 3,    1, 2, 3 );  // Vertices are shared multiple times with this method.
      }
      else
      { this.positions     .push( ...Vec.cast( [0,0,0], [1,0,0], [0,1,0],         // Method 2:  A tetrahedron with 
                                               [0,0,0], [1,0,0], [0,0,1],         // four independent triangles.
                                               [0,0,0], [0,1,0], [0,0,1],
                                               [0,0,1], [1,0,0], [0,1,0] ) );

        this.normals       .push( ...Vec.cast( [0,0,-1], [0,0,-1], [0,0,-1],        // This here makes Method 2 flat shaded, since values of
                                               [0,-1,0], [0,-1,0], [0,-1,0],        // normal vectors can be constant per whole triangle.
                                               [-1,0,0], [-1,0,0], [-1,0,0],        // Repeat them for all three vertices.
                                               [ a,a,a], [ a,a,a], [ a,a,a] ) );

        this.texture_coords.push( ...Vec.cast( [0,0], [1,0], [1,0],      // Each face in Method 2 also gets its own set of texture coords
                                               [0,0], [1,0], [1,0],      //(half the image is mapped onto each face).  We couldn't do this
                                               [0,0], [1,0], [1,0],      // with shared vertices since this features abrupt transitions
                                               [0,0], [1,0], [1,0] ) );  // when approaching the same point from different directions.

        this.indices.push( 0, 1, 2,    3, 4, 5,    6, 7, 8,    9, 10, 11 );      // Notice all vertices are unique this time.
      }
    }
}

  // *********** WINDMILL ***********
class Windmill extends Shape   // As our shapes get more complicated, we begin using matrices and flow
{ constructor( num_blades )   // control (including loops) to generate non-trivial point clouds and connect them.
    { super();
      for( var i = 0; i < num_blades; i++ )     // A loop to automatically generate the triangles.
        {
            var spin = Mat4.rotation( i * 2*Math.PI/num_blades, Vec.of( 0, 1, 0 ) );            // Rotate around a few degrees in XZ plane to place each new point.
            var newPoint  = spin.times( Vec.of( 1, 0, 0 ).to4(1) ).to3();   // Apply that XZ rotation matrix to point (1,0,0) of the base triangle.
            this.positions.push( newPoint,                         // Store this XZ position.                  This is point 1.
                                 newPoint.plus( [ 0, 1, 0 ] ),     // Store it again but with higher y coord:  This is point 2.
                                 Vec.of( 0, 0, 0 )            );  // All triangles touch this location.       This is point 3.

            // Rotate our base triangle's normal (0,0,1) to get the new one.  Careful!  Normal vectors are not points; their perpendicularity constraint
            // gives them a mathematical quirk that when applying matrices you have to apply the transposed inverse of that matrix instead.  But right 
            // now we've got a pure rotation matrix, where the inverse and transpose operations cancel out.
            var newNormal = spin.times( Vec.of( 0, 0, 1 ).to4(0) ).to3();  
            this.normals       .push( newNormal, newNormal, newNormal             );
            this.texture_coords.push( ...Vec.cast( [ 0, 0 ], [ 0, 1 ], [ 1, 0 ] ) );
            this.indices       .push( 3*i, 3*i + 1, 3*i + 2                       ); // Procedurally connect the three new vertices into triangles.
        }
    }
}

class Subdivision_Sphere extends Shape  // A subdivision surface ( see Wikipedia article ) is initially simple, then builds itself into a more and more detailed shape of the same
{                                 // layout.  Each act of subdivision makes it a better approximation of some desired mathematical surface by projecting each new
                                  // point onto that surface's known implicit equation.  For a sphere, we begin with a closed 3-simplex (a tetrahedron).  For 
                                  // each face, connect the midpoints of each edge together to make more faces.  Repeat recursively until the desired level of 
  constructor( max_subdivisions ) // detail is obtained.  Project all new vertices to unit vectors (onto the unit sphere) and group them into triangles by 
    { super();                    // following the predictable pattern of the recursion.
      this.positions.push( ...Vec.cast( [ 0, 0, -1 ], [ 0, .9428, .3333 ], [ -.8165, -.4714, .3333 ], [ .8165, -.4714, .3333 ] ) );  // Start with this equilateral tetrahedron
      
      this.subdivideTriangle( 0, 1, 2, max_subdivisions);  // Begin recursion.
      this.subdivideTriangle( 3, 2, 1, max_subdivisions);
      this.subdivideTriangle( 1, 0, 3, max_subdivisions);
      this.subdivideTriangle( 0, 2, 3, max_subdivisions); 
      
      for( let p of this.positions )
        { this.normals       .push( Vec.of( ...p ) );    // Each point has a normal vector that simply goes to the point from the origin.  Copy array by value.
          this.texture_coords.push( Vec.of( .5 + Math.atan2( p[2], p[0] ) / 2 / Math.PI, .5 - 2 * Math.asin( p[1] ) / 2 / Math.PI ) ); }
    }
  subdivideTriangle( a, b, c, count )   // Recurse through each level of detail by splitting triangle (a,b,c) into four smaller ones.
    { 
      if( count <= 0) { this.indices.push(a,b,c); return; }  // Base case of recursion - we've hit the finest level of detail we want.
                  
      var ab_vert = this.positions[a].mix( this.positions[b], 0.5).normalized(),     // We're not at the base case.  So,
          ac_vert = this.positions[a].mix( this.positions[c], 0.5).normalized(),     // build 3 new vertices at midpoints, and extrude them out to
          bc_vert = this.positions[b].mix( this.positions[c], 0.5).normalized();     // touch the unit sphere (length 1).
            
      var ab = this.positions.push( ab_vert ) - 1,      // Here, push() returns the indices of the three new vertices (plus one).
          ac = this.positions.push( ac_vert ) - 1,  
          bc = this.positions.push( bc_vert ) - 1;  
      
      this.subdivideTriangle( a, ab, ac,  count - 1 );      // Recurse on four smaller triangles, and we're done.
      this.subdivideTriangle( ab, b, bc,  count - 1 );      // Skipping every fourth vertex index in our list takes you down one level of detail, and 
      this.subdivideTriangle( ac, bc, c,  count - 1 );      // so on, due to the way we're building it.
      this.subdivideTriangle( ab, bc, ac, count - 1 );
    }
}

class Cube extends Shape    // A cube inserts six square strips into its arrays.
{ constructor()  
    { super();
      for( var i = 0; i < 3; i++ )                    
        for( var j = 0; j < 2; j++ )
        { var square_transform = Mat4.rotation( i == 0 ? Math.PI/2 : 0, Vec.of(1, 0, 0) ).times( Mat4.rotation( Math.PI * j - ( i == 1 ? Math.PI/2 : 0 ), Vec.of( 0, 1, 0 ) ) )
                                                                                         .times( Mat4.translation([ 0, 0, 1 ]) );
          Square.prototype.insert_transformed_copy_into( this, [], square_transform );             
        } 
    } 
}
  
class Phong_Model extends Shader          // ******* THE DEFAULT SHADER: Phong Reflection Model with Gouraud option ******* (also see Wikipedia article)
// The "vertex_glsl_code" string below is code that is sent to the graphics card at runtime, where on each run it gets compiled and linked there.  Thereafter, all of your 
// calls to draw shapes will launch the vertex shader program once per vertex in the shape (three times per triangle), sending results on to the next phase.  The purpose
// of this vertex shader program is to calculate the final resting place of vertices in screen coordinates; each of them starts out in local object coordinates.

// Likewise, the "fragment_glsl_code" string is used as the Fragment Shader program, which gets sent to the graphics card at runtime.  The fragment shader runs once all 
// the vertices in a triangle / element finish their vertex shader programs, and thus have finished finding out where they land on the screen.  The fragment shader fills
// in (shades) every pixel (fragment) overlapping where the triangle landed.  At each pixel it interpolates different values from the three extreme points of the triangle, 
// and uses them in formulas to determine color.  The fragment colors may or may not become final pixel colors; there could already be other triangles' fragments occupying 
// the same pixels.  The Z-Buffer test is applied to see if the new triangle is closer to the camera, and even if so, blending settings may interpolate some of the old color 
// into the result.
{ material(    color, ambient, diffusivity, shininess, smoothness, texture_object )        // Define the standard settings found in a phong lighting model.
    { return { color, ambient, diffusivity, shininess, smoothness, texture_object, shader: this } }
  shared_glsl_code()            // ********* SHARED CODE INCLUDED IN BOTH SHADERS *********
    { return `
        precision mediump float;
        const int N_LIGHTS = 2;                                                         // Be sure to keep this line up to date as you add more lights
        uniform float ambient, diffusivity, shininess, smoothness, animation_time, attenuation_factor[N_LIGHTS];
        uniform bool GOURAUD, COLOR_NORMALS, COLOR_VERTICES, USE_TEXTURE;               // Flags for alternate shading methods
        uniform vec4 lightPosition[N_LIGHTS], lightColor[N_LIGHTS], shapeColor;
        varying vec3 N, E, screen_space_pos;            // Specifier "varying" means it will be passed from the vertex shader on to the fragment shader, 
        varying vec2 f_tex_coord;                       // then interpolated per-fragment, weighted by the pixel fragment's proximity to each of the 3 vertices.          
        varying vec4 VERTEX_COLOR;
        varying vec3 L[N_LIGHTS], H[N_LIGHTS];
        varying float dist[N_LIGHTS];
        
        vec3 phong_model_lights( vec3 N )
          { vec3 result = vec3(0.0);
            for(int i = 0; i < N_LIGHTS; i++)
              {
                float attenuation_multiplier = 1.0 / (1.0 + attenuation_factor[i] * (dist[i] * dist[i]));
                float diffuse  =      max( dot(N, L[i]), 0.0 );
                float specular = pow( max( dot(N, H[i]), 0.0 ), smoothness );

                result += attenuation_multiplier * ( shapeColor.xyz * diffusivity * diffuse + lightColor[i].xyz * shininess * specular );
              }
            return result;
          }
        `;
    }
  vertex_glsl_code()           // ********* VERTEX SHADER *********
    { return `
        attribute vec4 color;
        attribute vec3 object_space_pos, normal;
        attribute vec2 tex_coord;

        uniform mat4 camera_transform, camera_model_transform, projection_camera_model_transform;
        uniform mat3 inverse_transpose_modelview;

        void main()
        { gl_Position = projection_camera_model_transform * vec4(object_space_pos, 1.0);      // The vertex's final resting place onscreen in normalized coords.
          N = normalize( inverse_transpose_modelview * normal );                              // The final normal vector in screen space.
          f_tex_coord = tex_coord;                                                            // Directly use original texture coords to make a "varying" texture coord.
          
          if( COLOR_NORMALS || COLOR_VERTICES )                                               // Bypass all lighting code if we're lighting up vertices some other way.
          { VERTEX_COLOR = COLOR_NORMALS ? ( vec4( N[0] > 0.0 ? N[0] : sin( animation_time * 3.0   ) * -N[0],             // In normals mode, rgb color = xyz quantity.  
                                                   N[1] > 0.0 ? N[1] : sin( animation_time * 15.0  ) * -N[1],             // Flash if it's negative.
                                                   N[2] > 0.0 ? N[2] : sin( animation_time * 45.0  ) * -N[2] , 1.0 ) ) : color;
            return;
          }
                                                                                // The rest of this shader calculates some quantities that the Fragment shader will need:
          screen_space_pos = ( camera_model_transform * vec4(object_space_pos, 1.0) ).xyz;
          E = normalize( -screen_space_pos );

          for( int i = 0; i < N_LIGHTS; i++ )
          {
            L[i] = normalize( ( camera_transform * lightPosition[i] ).xyz - lightPosition[i].w * screen_space_pos );   // Use w = 0 for a directional light source -- a 
            H[i] = normalize( L[i] + E );                                                                              // vector instead of a point.
                                                    // Is it a point light source?  Calculate the distance to it from the object.  Otherwise use some arbitrary distance.
            dist[i]  = lightPosition[i].w > 0.0 ? distance((camera_transform * lightPosition[i]).xyz, screen_space_pos)
                                                : distance( attenuation_factor[i] * -lightPosition[i].xyz, object_space_pos.xyz );
          }

          if( GOURAUD )         // Gouraud shading mode?  If so, finalize the whole color calculation here in the vertex shader, one per vertex, before we even 
          {                     // break it down to pixels in the fragment shader.   As opposed to Smooth "Phong" Shading, where we do calculate it afterwards.
            VERTEX_COLOR      = vec4( shapeColor.xyz * ambient, shapeColor.w);
            VERTEX_COLOR.xyz += phong_model_lights( N );
          }
        }`;
    }                            // A fragment is a pixel that's overlapped by the current triangle.  Fragments affect the final image or get discarded due to depth.
  fragment_glsl_code()           // ********* FRAGMENT SHADER ********* 
    { return `
        uniform sampler2D texture;
        void main()
        {
          if( GOURAUD || COLOR_NORMALS )    // Bypass Smooth "Phong" shading if, as in Gouraud case, we already have final colors to smear (interpolate) across vertices.
          {
            gl_FragColor = VERTEX_COLOR;
            return;
          }                                 // Calculate Smooth "Phong" Shading (not to be confused with the Phong Reflection Model).  As opposed to Gouraud Shading.
          vec4 tex_color = texture2D( texture, f_tex_coord );                         // Use texturing as well
          gl_FragColor      = tex_color * ( USE_TEXTURE ? ambient : 0.0 ) + vec4( shapeColor.xyz * ambient, USE_TEXTURE ? shapeColor.w * tex_color.w : shapeColor.w ) ;
          gl_FragColor.xyz += phong_model_lights( N );
        }`;
    }
  update_GPU( g_state, model_transform, material, gpu = this.g_addrs, gl = this.gl )     // Define how to synchronize our javascript's variables to the GPU's:
    { 
      this.update_matrices( g_state, model_transform, gpu, gl );    // (Send the matrices, additionally cache-ing some products of them we know we'll need.)
      gl.uniform1f ( gpu.animation_time_loc, g_state.animation_time / 1000 );

      if( g_state.gouraud === undefined ) { g_state.gouraud = g_state.color_normals = false; }    // (Keep the flags seen by the shader program
      gl.uniform1i( gpu.GOURAUD_loc,        g_state.gouraud       );                              //  up-to-date and make sure they are declared.)
      gl.uniform1i( gpu.COLOR_NORMALS_loc,  g_state.color_normals );

      gl.uniform4fv( gpu.shapeColor_loc,     material.color       );    // (Send the desired shape-wide material qualities to the graphics card)
      gl.uniform1f ( gpu.ambient_loc,        material.ambient     ); 
      gl.uniform1f ( gpu.diffusivity_loc,    material.diffusivity );
      gl.uniform1f ( gpu.shininess_loc,      material.shininess   );
      gl.uniform1f ( gpu.smoothness_loc,     material.smoothness  );

      if( material.texture_object )  // (Omit the texture parameter to signal not to draw a texture.)
      { gpu.shader_attributes[2].enabled = true;
        gl.uniform1f ( gpu.USE_TEXTURE_loc, 1 );
        gl.bindTexture( gl.TEXTURE_2D, material.texture_object.id );
      }
      else  { gl.uniform1f ( gpu.USE_TEXTURE_loc, 0 );   gpu.shader_attributes[2].enabled = false; }

      if( !g_state.lights.length )  return;
      var lightPositions_flattened = [], lightColors_flattened = [], lightAttenuations_flattened = [];
      for( var i = 0; i < 4 * g_state.lights.length; i++ )
        { lightPositions_flattened                  .push( g_state.lights[ Math.floor(i/4) ].position[i%4] );
          lightColors_flattened                     .push( g_state.lights[ Math.floor(i/4) ].color[i%4] );
          lightAttenuations_flattened[ Math.floor(i/4) ] = g_state.lights[ Math.floor(i/4) ].attenuation;
        }
      gl.uniform4fv( gpu.lightPosition_loc,       lightPositions_flattened );
      gl.uniform4fv( gpu.lightColor_loc,          lightColors_flattened );
      gl.uniform1fv( gpu.attenuation_factor_loc,  lightAttenuations_flattened );
    }
  update_matrices( g_state, model_transform, gpu, gl )                                                  // Helper function for sending matrices to GPU
    { let [ P, C, M ]    = [ g_state.projection_transform, g_state.camera_transform, model_transform ],   // (PCM will mean Projection * Camera * Model)
            CM     =      C.times(  M ),
            PCM    =      P.times( CM ),                           // Send the current matrices to the shader.  Go ahead and pre-compute the products we'll 
            inv_CM = Mat4.inverse( CM ).sub_block([0,0], [3,3]);   // need of the of the three special matrices and just cache and send those.  They will be 
                                                                   // the same throughout this draw call & thus across each instance of the vertex shader.
      gl.uniformMatrix4fv( gpu.camera_transform_loc,                  false, Mat.flatten_2D_to_1D(     C .transposed() ) );    // GPU expects matrices as column-major arrays.
      gl.uniformMatrix4fv( gpu.camera_model_transform_loc,            false, Mat.flatten_2D_to_1D(     CM.transposed() ) );
      gl.uniformMatrix4fv( gpu.projection_camera_model_transform_loc, false, Mat.flatten_2D_to_1D(    PCM.transposed() ) );
      gl.uniformMatrix3fv( gpu.inverse_transpose_modelview_loc,       false, Mat.flatten_2D_to_1D( inv_CM              ) );       
    }
}    
      
class Funny_Shader extends Phong_Model    // Simple "procedural" texture shader without input image.  This borrows its vertex shader from Phong_Model.
{ material() { return { shader: this } }  // Materials here are minimal, without settings.
  update_GPU( g_state, model_transform, material, gpu = this.g_addrs, gl = this.gl )     // Send javascrpt's variables to the GPU to update its overall state.
      { this.update_matrices( g_state, model_transform, gpu, gl );
        gl.uniform1f ( gpu.animation_time_loc, g_state.animation_time / 1000 );
        gpu.shader_attributes[2].enabled = true;
      }
  fragment_glsl_code()           // ********* FRAGMENT SHADER *********
    { return `
        void main()
        { float a = animation_time, u = f_tex_coord.x, v = f_tex_coord.y;

          gl_FragColor = vec4(
            2.0 * u * sin(17.0 * u ) + 3.0 * v * sin(11.0 * v ) + 1.0 * sin(13.0 * a),
            3.0 * u * sin(18.0 * u ) + 4.0 * v * sin(12.0 * v ) + 2.0 * sin(14.0 * a),
            4.0 * u * sin(19.0 * u ) + 5.0 * v * sin(13.0 * v ) + 3.0 * sin(15.0 * a),
            5.0 * u * sin(20.0 * u ) + 6.0 * v * sin(14.0 * v ) + 4.0 * sin(16.0 * a));
        }`;
    }
}

class Movement_Controls extends Scene_Component    // A Scene_Component that our Canvas_Manager can manage.  Adds both first-person and third-person style camera matrix controls to the canvas.
{ constructor( context, canvas = context.canvas )
    { super( context );                            // Initialize some data members:
      Object.assign( this, { thrust: Vec.of(0,0,0), roll: 0, origin: Vec.of(0,5,0), look_around_locked: true, pos: Vec.of( 0,0,0 ), z_axis: Vec.of( 0,0,0 ) } );
      this.target = function() { return context.globals.movement_controls_target() }                                      // The camera matrix is not actually stored here inside Movement_Controls; instead, track
      this.target_is_a_camera = function() { return context.globals.movement_target_is_a_camera }                         // an external matrix to modify. This target is a reference (made with closures) kept
      context.globals.movement_controls_target = function(t) { return context.globals.graphics_state.camera_transform };  // in "globals" so it can be seen and set by other classes.  Initially, the default target
      context.globals.movement_target_is_a_camera = true;                                                                 // is a camera matrix stored in the global graphics_state object, for Shaders to use.
      
      // *** Mouse controls: ***
      this.mouse = { "from_center": Vec.of(0,0) };                           // Measure mouse steering, for rotating the flyaround camera:
      const mouse_position = function( e, rect = canvas.getBoundingClientRect() ) { return Vec.of( e.clientX - (rect.left + rect.right)/2, e.clientY - (rect.bottom + rect.top)/2 ); };        
      canvas.addEventListener( "mouseup",   ( function(self) { return function(e) { e = e || window.event;    self.mouse.anchor = undefined;              } } ) (this), false );
      canvas.addEventListener( "mousedown", ( function(self) { return function(e) { e = e || window.event;    self.mouse.anchor = mouse_position(e);      } } ) (this), false );
      canvas.addEventListener( "mousemove", ( function(self) { return function(e) { e = e || window.event;    self.mouse.from_center = mouse_position(e); } } ) (this), false );
      canvas.addEventListener( "mouseout",  ( function(self) { return function(e) { self.mouse.from_center = Vec.of(0,0); }; } ) (this), false );  // Stop reacting if the mouse leaves the canvas.
    }
  make_control_panel()   // This function of a scene sets up its keyboard shortcuts.
    { const globals = this.globals;
      this.control_panel.innerHTML += "Click and drag the scene to <br> spin your viewpoint around it.<br>";
      this.key_triggered_button( "Up",     "space", function() { this.thrust[1] = -1 }, undefined, function() { this.thrust[1] = 0 } );
      this.key_triggered_button( "Forward",    "w", function() { this.thrust[2] =  1 }, undefined, function() { this.thrust[2] = 0 } ); this.new_line();
      this.key_triggered_button( "Left",       "a", function() { this.thrust[0] =  1 }, undefined, function() { this.thrust[0] = 0 } );
      this.key_triggered_button( "Back",       "s", function() { this.thrust[2] = -1 }, undefined, function() { this.thrust[2] = 0 } );
      this.key_triggered_button( "Right",      "d", function() { this.thrust[0] = -1 }, undefined, function() { this.thrust[0] = 0 } ); this.new_line();
      this.key_triggered_button( "Down",       "z", function() { this.thrust[1] =  1 }, undefined, function() { this.thrust[1] = 0 } ); this.new_line();
      this.key_triggered_button( "Roll left",  ",", function() { this.roll      =  1 }, undefined, function() { this.roll      = 0 } );
      this.key_triggered_button( "Roll right", ".", function() { this.roll      = -1 }, undefined, function() { this.roll      = 0 } ); this.new_line();
      this.key_triggered_button( "(Un)freeze look around",   "f",       function() { this.look_around_locked  ^=  1 },    "green" );    this.new_line();
      this.live_string( () => { return "Position: "            + this.   pos[0].toFixed(2) + ", " + this.   pos[1].toFixed(2) + ", " + this.   pos[2].toFixed(2) } ); this.new_line();
      this.live_string( () => { return "Center of rotation: "  + this.origin[0].toFixed(0) + ", " + this.origin[1].toFixed(0) + ", " + this.origin[2].toFixed(0) } ); this.new_line();
      this.live_string( () => { return "Facing: " + ( ( this.z_axis[0] > 0 ? "West " : "East ")             // (Actually affected by the left hand rule)
                                                    + ( this.z_axis[1] > 0 ? "Down " : "Up " ) + ( this.z_axis[2] > 0 ? "North" : "South" ) ) } ); this.new_line();
      
      this.key_triggered_button( "Move spin center to here", "o",       function() { this.origin = Mat4.inverse( this.target() ).times( Vec.of(0,0,0,1) ).to3() },    "brown" ); this.new_line();
      this.key_triggered_button( "Go to world origin",       "r",       function() { this.target().set_identity( 4,4 ) }, "orange" ); this.new_line();
      this.key_triggered_button( "Reset target to main camera", "shift+r", function() { globals.movement_controls_target = function() { return globals.graphics_state.camera_transform }; globals.movement_target_is_a_camera = true; }, "blue" ); this.new_line();
    }
  first_person_effects             ( radians_per_frame, meters_per_frame, offsets_from_dead_box )
    { if( !this.look_around_locked ) 
        for( var i = 0; i < 2; i++ )      // Steer according to "mouse_from_center" vector, but don't start increasing until outside a leeway window from the center.
        { let o = offsets_from_dead_box, velocity = ( ( o.minus[i] > 0 && o.minus[i] ) || ( o.plus[i] < 0 && o.plus[i] ) ) * radians_per_frame;  // The &&'s can zero these out.
          this.target().post_multiply( Mat4.rotation( -velocity, Vec.of( i, 1-i, 0 ) ) );   // On X step, rotate around Y axis, and vice versa.
        }
      if( this.roll != 0 ) this.target().post_multiply( Mat4.rotation( -.1, Vec.of(0, 0, this.roll ) ) );
      this.target().post_multiply( Mat4.translation( this.thrust.times( -meters_per_frame ) ) ); // Now apply translation movement of the camera, in the newest local coordinate frame
    }
  first_person_effects_but_inverted( radians_per_frame, meters_per_frame, offsets_from_dead_box )
    { if( !this.look_around_locked ) 
        for( var i = 0; i < 2; i++ )      // Steer according to "mouse_from_center" vector, but don't start increasing until outside a leeway window from the center.
        { let o = offsets_from_dead_box, velocity = ( ( o.minus[i] > 0 && o.minus[i] ) || ( o.plus[i] < 0 && o.plus[i] ) ) * radians_per_frame;  // The &&'s can zero these out.
          this.target().pre_multiply( Mat4.rotation( +velocity, Vec.of( i, 1-i, 0 ) ) );   // On X step, rotate around Y axis, and vice versa.
        }
      if( this.roll != 0 ) this.target().pre_multiply( Mat4.rotation( +.1, Vec.of(0, 0, this.roll ) ) );
      this.target().pre_multiply( Mat4.translation( this.thrust.times( +meters_per_frame ) ) ); // Now apply translation movement of the camera, in the newest local coordinate frame
    }
  third_person_effects             ( radians_per_frame, dragging_vector )
    { this.target().pre_multiply( Mat4.translation( this.origin.times(-1)  ) )   // Post-multiply so we rotate the scene instead of the camera.
                   .pre_multiply( Mat4.rotation( -radians_per_frame * dragging_vector.norm(), Vec.of( dragging_vector[1], dragging_vector[0], 0 ) ) )
                   .pre_multiply( Mat4.translation( this.origin            ) );
    }
  third_person_effects_but_inverted( radians_per_frame, dragging_vector )
    { this.target().post_multiply( Mat4.translation( this.origin           ) )   // Post-multiply so we rotate the scene instead of the camera.
                   .post_multiply( Mat4.rotation( +radians_per_frame * dragging_vector.norm(), Vec.of( dragging_vector[1], dragging_vector[0], 0 ) ) )
                   .post_multiply( Mat4.translation( this.origin.times(-1) ) );
    }
  first_person_flyaround( radians_per_frame, meters_per_frame, leeway = 70 )  // Determine camera rotation movement when the mouse is past a minimum distance (leeway) from the canvas's center.
    { var offsets_from_dead_box = { plus:  [ this.mouse.from_center[0] + leeway, this.mouse.from_center[1] + leeway ],
                                    minus: [ this.mouse.from_center[0] - leeway, this.mouse.from_center[1] - leeway ] };    // Compare mouse's location to all four corners of dead box.
      if( this.target_is_a_camera() ) this.first_person_effects_but_inverted( radians_per_frame, meters_per_frame, offsets_from_dead_box );
      else                            this.first_person_effects             ( radians_per_frame, meters_per_frame, offsets_from_dead_box );
    }
  third_person_arcball( radians_per_frame )
    { let dragging_vector = this.mouse.from_center.minus( this.mouse.anchor );  // Spin the scene around the world origin on a user-determined axis.
      if( dragging_vector.norm() > 0 ) 
        if( this.target_is_a_camera() ) this.third_person_effects_but_inverted( radians_per_frame, dragging_vector );
        else                            this.third_person_effects             ( radians_per_frame, dragging_vector );
    }
  display( graphics_state, dt = graphics_state.animation_delta_time )           // Camera code starts here.
    { this.first_person_flyaround( dt/100000, dt/50 );                          // Scale the normal camera aiming speed by dt for smoothness.
      if( this.mouse.anchor ) this.third_person_arcball( dt/30000 );            // Also apply third-person "arcball" camera mode if a mouse drag is occurring.  
      
      const inv = Mat4.inverse( this.target() ); this.pos = inv.times( Vec.of( 0,0,0,1 ) ); this.z_axis = inv.times( Vec.of( 0,0,1,0 ) );      // Log some values.
    }
}

class Global_Info_Table extends Scene_Component  // A class that just toggles, monitors, and reports some global values via its control panel.
{ make_control_panel()
    { const globals = this.globals;
      this.control_panel.innerHTML += "Any values placed in this <br> scratchpad can be accessed <br> by all Scene_Components. <br>";
      this.key_triggered_button( "(Un)pause animation", "ALT+a", function() { globals.animate ^= 1; } ); this.new_line();
      this.live_string( () => { return "Animation Time: " + ( globals.graphics_state.animation_time/1000 ).toFixed(3) + "s" } );
      this.live_string( () => { return globals.animate ? " " : " (paused)" } );  this.new_line();
      this.key_triggered_button( "Gouraud shading",     "ALT+g", function() { globals.graphics_state.gouraud       ^= 1;         } ); this.new_line();
      this.key_triggered_button( "Normals shading",     "ALT+n", function() { globals.graphics_state.color_normals ^= 1;         } ); this.new_line();
    }
}

// surface demo
class Grid_Patch extends Shape                          // A deformed grid of rows and columns. A tesselation of triangles connects the points, by generating a certain
{  constructor( rows, columns, next_row_function, next_column_function, texture_coord_range = [ [ 0, rows ], [ 0, columns ] ]  )      // predictable pattern of indices.
{ super();                                                                     // Two callbacks allow you to dynamically define how to reach the next row or column.
    let points = [];
    for( let r = 0; r <= rows; r++ )
    { points.push( new Array( columns+1 ) );                                                  // Allocate a 2D array
        points[ r ][ 0 ] = next_row_function( r/rows, points[ r-1 ] && points[ r-1 ][ 0 ] );    // Use next_row_function to generate the start point of each row.
    }                                                                                         //   Include the previous point if it existed.
    for(   let r = 0; r <= rows;    r++ )
        for( let c = 0; c <= columns; c++ )
        { if( c > 0 ) points[r][ c ] = next_column_function( c/columns, points[r][ c-1 ], r/rows );           // From those, use next_column function to generate the remaining points.

            this.positions.push( points[r][ c ] );
            const a1 = c/columns, a2 = r/rows, x_range = texture_coord_range[0], y_range = texture_coord_range[1];
            this.texture_coords.push( Vec.of( ( a1 )*x_range[1] + ( 1-a1 )*x_range[0], ( a2 )*y_range[1] + ( 1-a2 )*y_range[0] ) );    // Interpolate texture coords from a range.
        }
    for(   let r = 0; r <= rows;    r++ )                                                   // Generate normals by averaging the cross products of all defined neighbor pairs.
        for( let c = 0; c <= columns; c++ )
        { let curr = points[r][c], neighbors = new Array(4), normal = Vec.of( 0,0,0 );
            for( let [ i, dir ] of [ [ -1,0 ], [ 0,1 ], [ 1,0 ], [ 0,-1 ] ].entries() )         // Store each neighbor by rotational order.
                neighbors[i] = points[ r + dir[1] ] && points[ r + dir[1] ][ c + dir[0] ];        // Leave "undefined" in the array wherever we hit a boundary.

            for( let i = 0; i < 4; i++ )                                                        // Cross pairs of neighbors, proceeding the same rotational direction through the pairs.
                if( neighbors[i] && neighbors[ (i+1)%4 ] ) normal = normal.plus( neighbors[i].minus( curr ).cross( neighbors[ (i+1)%4 ].minus( curr ) ) );
            normal.normalize();                                                                 // Normalize the sum to get the average vector.

            if( normal.every( x => x == x ) && normal.norm() > .01 )  this.normals.push( Vec.from( normal ) );    // Store the normal if it's valid (not NaN or zero length)
        else                                                      this.normals.push( Vec.of( 0,0,1 )    );    // Otherwise use a default.
        }
    for( let i = 0; i < this.normals.length; i++ )
    { if( this.normals[i].norm() > 0 ) break;
        this.normals[i] = first_valid_normal;
    }

    for( var h = 0; h < rows; h++ )             // Generate a sequence like this (if #columns is 10):  "1 11 0  11 1 12  2 12 1  12 2 13  3 13 2  13 3 14  4 14 3..."
        for( var i = 0; i < 2 * columns; i++ )
            for( var j = 0; j < 3; j++ )
                this.indices.push( h * ( columns + 1 ) + columns * ( ( i + ( j % 2 ) ) % 2 ) + ( ~~( ( j % 3 ) / 2 ) ?
                    ( ~~( i / 2 ) + 2 * ( i % 2 ) )  :  ( ~~( i / 2 ) + 1 ) ) );
}
    static sample_array( array, ratio )                                           // Optional.  In an array of points, interpolate the pair of points that our progress ratio falls between.
    { const frac = ratio * ( array.length - 1 ), alpha = frac - Math.floor( frac );
        return array[ Math.floor( frac ) ].mix( array[ Math.ceil( frac ) ], alpha );
    }
}

class Surface_Of_Revolution extends Grid_Patch
    // SURFACE OF REVOLUTION: Produce a curved "sheet" of triangles with rows and columns.  Begin with an input array of points, defining a 1D path curving through 3D space --
    // now let each point be a row.  Sweep that whole curve around the Z axis in equal steps, stopping and storing new points along the way; let each step be a column.  Now we
    // have a flexible "generalized cylinder" spanning an area until total_curvature_angle.
{ constructor( rows, columns, points, texture_coord_range, total_curvature_angle = 2*Math.PI )
{ super( rows, columns, i => Grid_Patch.sample_array( points, i ), (j,p) => Mat4.rotation( total_curvature_angle/columns, Vec.of( 0,0,1 ) ).times(p.to4(1)).to3(), texture_coord_range );
}
}

class Half_Surface_Of_Revolution extends Grid_Patch
    // SURFACE OF REVOLUTION: Produce a curved "sheet" of triangles with rows and columns.  Begin with an input array of points, defining a 1D path curving through 3D space --
    // now let each point be a row.  Sweep that whole curve around the Z axis in equal steps, stopping and storing new points along the way; let each step be a column.  Now we
    // have a flexible "generalized cylinder" spanning an area until total_curvature_angle.
{ constructor( rows, columns, points, texture_coord_range, total_curvature_angle = Math.PI )
  { super( rows, columns, i => Grid_Patch.sample_array( points, i ), (j,p) => Mat4.rotation( total_curvature_angle/columns, Vec.of( 0,0,1 ) ).times(p.to4(1)).to3(), texture_coord_range );
  }
}

class Regular_2D_Polygon extends Surface_Of_Revolution  // Approximates a flat disk / circle
{ constructor( rows, columns ) { super( rows, columns, [ ...Vec.cast( [0, 0, 0], [1, 0, 0] ) ] );
    this.normals = this.normals.map( x => Vec.of( 0,0,1 ) );
    this.texture_coords.forEach( (x, i, a) => a[i] = this.positions[i].map( x => x/2 + .5 ).slice(0,2) );
  }
}

class Half_Regular_2D_Polygon extends Half_Surface_Of_Revolution  // Approximates a flat disk / circle
{ constructor( rows, columns ) { super( rows, columns, [ ...Vec.cast( [0, 0, 0], [1, 0, 0] ) ] );
    this.normals = this.normals.map( x => Vec.of( 0,0,1 ) );
    this.texture_coords.forEach( (x, i, a) => a[i] = this.positions[i].map( x => x/2 + .5 ).slice(0,2) );
  }
}

// columns: edges; rows >= 1, change height = 2
class Cylindrical_Tube extends Surface_Of_Revolution    // An open tube shape with equally sized sections, pointing down Z locally.
{ constructor( rows, columns, texture_range ) { super( rows, columns, [ ...Vec.cast( [1, 0, 1], [1, 0, -1] ) ], texture_range ); } }

class Cone_Tip extends Surface_Of_Revolution            // Note:  Curves that touch the Z axis degenerate from squares into triangles as they sweep around
{ constructor( rows, columns, texture_range ) { super( rows, columns, [ ...Vec.cast( [0, 0, 1],  [1, 0, -1]  ) ], texture_range ); } }

class Torus extends Shape
{ constructor( rows, columns, texture_range )
{ super();
    let circle_points = Array( rows ).fill( Vec.of( .5,0,0 ) );
    circle_points = circle_points.map( (x,i,a) => Mat4.rotation( i/(a.length-1) * 2*Math.PI, Vec.of( 0,-1,0 ) ).times( x.to4(1) ).to3() );
    circle_points = circle_points.map( (x,i,a) => Mat4.translation([ -.75,0,0 ]).times( x.to4(1) ).to3() );

    Surface_Of_Revolution.prototype.insert_transformed_copy_into( this, [ rows, columns, circle_points, texture_range ] );
} }

class Grid_Sphere extends Shape           // With lattitude / longitude divisions; this means singularities are at
{ constructor( rows, columns, texture_range )             // the mesh's top and bottom.  Subdivision_Sphere is a better alternative.
{ super();
    let semi_circle_points = Array( rows ).fill( Vec.of( 0,0,1 ) );
    semi_circle_points = semi_circle_points.map( (x,i,a) => Mat4.rotation( i/(a.length-1) * Math.PI, Vec.of( 0,1,0 ) ).times( x.to4(1) ).to3() );

    Surface_Of_Revolution.prototype.insert_transformed_copy_into( this, [ rows, columns, semi_circle_points, texture_range ] );
} }

class Closed_Cone extends Shape     // Combine a cone tip and a regular polygon to make a closed cone.
{ constructor( rows, columns, texture_range )
{ super();
    Cone_Tip          .prototype.insert_transformed_copy_into( this, [ rows, columns, texture_range ]);
    Regular_2D_Polygon.prototype.insert_transformed_copy_into( this, [ 1, columns ], Mat4.rotation( Math.PI, Vec.of(0, 1, 0) ).times( Mat4.translation([ 0, 0, 1 ]) ) ); } }

class Rounded_Closed_Cone extends Surface_Of_Revolution   // An alternative without two separate sections
{ constructor( rows, columns, texture_range ) { super( rows, columns, [ ...Vec.cast( [0, 0, 1], [1, 0, -1], [0, 0, -1] ) ], texture_range ) ; } }

class Capped_Cylinder extends Shape   // Combine a tube and two regular polygons to make a closed cylinder.  Flat shade this to make a prism, where #columns = #sides.
{ constructor( rows, columns, texture_range )
{ super();
    Cylindrical_Tube  .prototype.insert_transformed_copy_into( this, [ rows, columns, texture_range ] );
    Regular_2D_Polygon.prototype.insert_transformed_copy_into( this, [ 1, columns ], Mat4.translation([ 0, 0, .5 ]).times(Mat4.scale([.5,.5,1])) );
    Regular_2D_Polygon.prototype.insert_transformed_copy_into( this, [ 1, columns ], Mat4.rotation( Math.PI, Vec.of(0, 1, 0) ).times( Mat4.translation([ 0, 0, .5 ]) ) ); } }

class Rounded_Capped_Cylinder extends Surface_Of_Revolution   // An alternative without three separate sections
{ constructor ( rows, columns, texture_range ) { super( rows, columns, [ ...Vec.cast( [0, 0, .5], [1, 0, .5], [1, 0, -.5], [0, 0, -.5] ) ], texture_range ); } }

class Axis_Arrows extends Shape   // An axis set made out of a lot of various primitives.
{ constructor()
{ super();
    var stack = [];
    Subdivision_Sphere.prototype.insert_transformed_copy_into( this, [ 3 ], Mat4.rotation( Math.PI/2, Vec.of( 0,1,0 ) ).times( Mat4.scale([ .25, .25, .25 ]) ) );
    this.drawOneAxis( Mat4.identity(),                                                            [[  0 ,.33 ], [ 0,1 ]] );
    this.drawOneAxis( Mat4.rotation(-Math.PI/2, Vec.of(1,0,0)).times( Mat4.scale([  1, -1, 1 ])), [[ .34,.66 ], [ 0,1 ]] );
    this.drawOneAxis( Mat4.rotation( Math.PI/2, Vec.of(0,1,0)).times( Mat4.scale([ -1,  1, 1 ])), [[ .67, 1  ], [ 0,1 ]] );
}
    drawOneAxis( transform, tex )    // Use a different texture coordinate range for each of the three axes, so they show up differently.
    { Closed_Cone     .prototype.insert_transformed_copy_into( this, [ 4, 10, tex ], transform.times( Mat4.translation([   0,   0,  2 ]) ).times( Mat4.scale([ .25, .25, .25 ]) ), 0 );
        Cube            .prototype.insert_transformed_copy_into( this, [ ],            transform.times( Mat4.translation([ .95, .95, .45]) ).times( Mat4.scale([ .05, .05, .45 ]) ), 0 );
        Cube            .prototype.insert_transformed_copy_into( this, [ ],            transform.times( Mat4.translation([ .95,   0, .5 ]) ).times( Mat4.scale([ .05, .05, .4  ]) ), 0 );
        Cube            .prototype.insert_transformed_copy_into( this, [ ],            transform.times( Mat4.translation([   0, .95, .5 ]) ).times( Mat4.scale([ .05, .05, .4  ]) ), 0 );
        Cylindrical_Tube.prototype.insert_transformed_copy_into( this, [ 7, 7,  tex ], transform.times( Mat4.translation([   0,   0,  1 ]) ).times( Mat4.scale([  .1,  .1,  2  ]) ), 0 );
    }
}

// picture sides should be power of two
class Fake_Bump_Map extends Phong_Model  // Overrides Phong_Model except for one thing
{ fragment_glsl_code()           // ********* FRAGMENT SHADER *********
{ return `
        uniform sampler2D texture;          //  Like real bump mapping, but with no separate file for the bump map (instead we'll
        void main()                         //  re-use the colors of the original picture file to disturb the normal vectors)
        {
          if( GOURAUD || COLOR_NORMALS )    // Bypass Smooth "Phong" shading if, as in Gouraud case, we already have final colors to smear (interpolate) across vertices.
          {
            gl_FragColor = VERTEX_COLOR;
            return;
          }                                 // Calculate Smooth "Phong" Shading (not to be confused with the Phong Reflection Model).  As opposed to Gouraud Shading.
          vec4 tex_color = texture2D( texture, f_tex_coord );                         // Use texturing as well
          vec3 bumped_N  = normalize( N + tex_color.rgb - .5*vec3(1,1,1) );           // Slightly disturb normals based on sampling the same texture
          gl_FragColor      = tex_color * ( USE_TEXTURE ? ambient : 0.0 ) + vec4( shapeColor.xyz * ambient, USE_TEXTURE ? shapeColor.w * tex_color.w : shapeColor.w ) ;
          gl_FragColor.xyz += phong_model_lights( bumped_N );
        }`;
}
}
class Surfaces_Demo extends Scene_Component
{ constructor( context )
{ super( context );

    let square_array = Vec.cast( [ 1,0,-1 ], [ 0,1,-1 ], [ -1,0,-1 ], [ 0,-1,-1 ], [ 1,0,-1 ] ),               // Some helper arrays of points located along
        star_array = Array(19).fill( Vec.of( 1,0,-1 ) ), circle_array = Array(40).fill( Vec.of( 1,0,-1 ) );  // curves.  We'll extrude these into surfaces.
    circle_array = circle_array.map( (x,i,a) => Mat4.rotation( i/(a.length-1) * 2*Math.PI, Vec.of( 0,0,1 ) ).times( x.to4(1) ).to3() );
    star_array   =   star_array.map( (x,i,a) => Mat4.rotation( i/(a.length-1) * 2*Math.PI, Vec.of( 0,0,1 ) ).times( Mat4.translation([ (i%2)/2,0,0 ]) ).times( x.to4(1) ).to3() );

    let sin_rows_func       =      i  => { return Vec.of( .5 + Math.sin(777*i)/4, 2-4*i, 0 ) },                                   // Different callbacks for telling Grid_Patch
    sin_columns_func    = ( j,p ) => { return Mat4.translation([ Math.sin(777*j)/4,0,4/30    ]).times( p.to4(1) ).to3() },    // how it should advance to the next row/column.
    rotate_columns_func = ( j,p ) => { return Mat4.rotation( .1*j*Math.PI, Vec.of( 0,1,0 )    ).times( p.to4(1) ).to3() },
    sample_square_func  =      i  => { return Grid_Patch.sample_array( square_array, i ) },
    sample_star_func    =      i  => { return Grid_Patch.sample_array( star_array,   i ) },
    sample_circle_func  =      i  => { return Grid_Patch.sample_array( circle_array, i ) },
    sample_two_arrays   = (j,p,i) => { return Mat4.translation([0,0,2*j]).times( sample_star_func(i).mix( sample_circle_func(i), j ).to4(1) ).to3() },
    sample_two_arrays2  = (j,p,i) => { return Mat4.rotation( .5*j*Math.PI, Vec.of( 1,1,1 ) ).times(
    Mat4.translation([0,0,2*j]).times( sample_star_func(i).mix( sample_square_func(i), j ).to4(1) ) ).to3() },
    line_rows_func      = ( i,p ) => { return p ? Mat4.translation([0,i/50,0]).times( p.to4(1) ).to3() :  Vec.of( .01,-.05,-.1 ) },
    transform_cols_func = (j,p,i) => { return Mat4.rotation( Math.PI/8, Vec.of( 0,0,1 ) ).times( Mat4.scale([ 1.1,1.1,1.1 ])).times( Mat4.translation([ 0,0,.005 ]))
    .times( p.to4(1) ).to3() };
    var shapes = {
        // good_sphere : new Subdivision_Sphere( 4 ),                                           // A sphere made of nearly equilateral triangles / no singularities
        // vase        : new Grid_Patch( 30, 30, sin_rows_func, rotate_columns_func,   [[0,1],[0,1]] ),
        // box         : new Cube(),
        // ghost       : new Grid_Patch( 36, 10, sample_star_func, sample_two_arrays,  [[0,1],[0,1]] ),
        // shell       : new Grid_Patch( 10, 40, line_rows_func, transform_cols_func,  [[0,5],[0,1]] ),
        // waves       : new Grid_Patch( 30, 30, sin_rows_func, sin_columns_func,      [[0,1],[0,1]] ),
        // shell2      : new Grid_Patch( 30, 30, sample_star_func, sample_two_arrays2, [[0,1],[0,1]] ),
        // tube        : new Cylindrical_Tube  ( 10, 3, [[0,1],[0,1]] ),
        // open_cone   : new Cone_Tip          (  3, 10, [[0,1],[0,1]] ),
        // donut       : new Torus             ( 30, 30 ),
        // gem2        : new ( Torus             .prototype.make_flat_shaded_version() )( 20, 20 ),
        // bad_sphere  : new Grid_Sphere       ( 10, 10 ),                                            // A sphere made of rows and columns, with singularities
        // septagon    : new Regular_2D_Polygon(  2,  7 ),
        // cone        : new Closed_Cone       ( 4, 5, [[0,1],[0,1]] ),                       // Cone.  Useful.
        capped      : new Capped_Cylinder   ( 4, 5, [[0,1],[0,1]] ),                       // Cylinder.  Also useful.
        // axis        : new Axis_Arrows(),                                                    // Axis.  Draw them often to check your current basis.
        // prism       : new ( Capped_Cylinder   .prototype.make_flat_shaded_version() )( 10, 10, [[0,1],[0,1]] ),
        // gem         : new ( Subdivision_Sphere.prototype.make_flat_shaded_version() )(  2     ),
        //swept_curve : new Surface_Of_Revolution( 10, 10, [ ...Vec.cast( [2, 0, -1], [1, 0, 0], [1, 0, 1], [0, 0, 2] ) ], [ [ 0, 1 ], [ 0, 7 ] ], Math.PI/3 ),
    };
    this.submit_shapes( context, shapes );
    Object.assign( context.globals.graphics_state, { camera_transform: Mat4.translation([ -2,2,-15 ]), projection_transform: Mat4.perspective( Math.PI/4, context.width/context.height, .1, 1000 ) } );
    Object.assign( this, { shader: context.get_instance( Fake_Bump_Map ), textures: [], gallery: false, patch_only: false, revolution_only: false } );
    for( let filename of [ "/assets/rgb.jpg", "/assets/stars.png", "/assets/earth.gif", "/assets/text.png" ] ) this.textures.push( context.get_instance( filename ) ); this.textures.push( undefined );
}
    display( graphics_state )
    { let model_transform = Mat4.identity(), t = graphics_state.animation_time / 1000;
        graphics_state.lights = [ new Light( Vec.of( 1,1,0, 0 ).normalized(), Color.of(  1, .5, .5, 1 ), 100000000 ),
            new Light( Vec.of( 0,1,0, 0 ).normalized(), Color.of( .5,  1, .5, 1 ), 100000000 ) ];

        for( var i = 0; i < 5; i++ )           // Draw some moving worm-like sequences of shapes.  Keep the matrix state from the previous one to draw the next one attached at the end.
        { let j = i;
            for( let key in this.shapes )
            { j++;
                if( this.patch_only      &&    this.shapes[ key ].constructor.name != "Grid_Patch"   ) continue;    // Filter some shapes out when those buttons have been pressed.
                if( this.revolution_only && !( this.shapes[ key ] instanceof Surface_Of_Revolution ) ) continue;

                let funny_function_of_time = t/5 + j*j*Math.cos( t/10 )/50,
                    random_material = this.shader.material( Color.of( (j % 6)/10, (j % 5)/10, (j % 4)/10, 1 ), .4, 1, 1, 40, this.textures[0] )

                model_transform.post_multiply( Mat4.rotation( funny_function_of_time, Vec.of(j%3 == 0, j%3 == 1, j%3 == 2) ) );   // Irregular motion
                if( this.gallery ) model_transform.pre_multiply ( Mat4.translation([ 3, 0,0 ]) );   // Gallery mode:  Switch the rotation/translation order to line up the shapes.
                else               model_transform.post_multiply( Mat4.translation([ 0,-3,0 ]) );
                this.shapes[ key ].draw( graphics_state, model_transform, random_material );        //  Draw the current shape in the list
            }
            model_transform.post_multiply( Mat4.rotation( .5, Vec.of(0, 0, 1) ) );
        }
    }
    make_control_panel()   // This function of a scene sets up its keyboard shortcuts.
    { this.key_triggered_button( "Next Texture",                   "t", function() { this.textures.push( this.textures.shift() )    },  "red" ); this.new_line();
        this.key_triggered_button( "Gallery View",                   "g", function() { this.gallery ^= 1;                             }, "blue" ); this.new_line();
        this.key_triggered_button( "Revolution Surfaces Only", "shift+S", function() { this.revolution_only = 1; this.patch_only = 0; }         ); this.new_line();
        this.key_triggered_button( "Custom Patches Only",      "shift+C", function() { this.revolution_only = 0; this.patch_only = 1; }         ); this.new_line();
        this.key_triggered_button( "All Shapes",               "shift+A", function() { this.revolution_only = 0; this.patch_only = 0; }         );
    }
    show_explanation( document_element )
    { document_element.innerHTML += "Welcome to the Surfaces Demo!  This is a demonstration of how to make a diverse set of shapes using the least amount of code. <br>"
        +  "Use the movement controls below to explore the scene.  You may find it easier to do this in gallery mode, entered by pressing "
        +  "g or the blue button below.  Press t to cycle through the loaded texture images.<br>"
        +  "A cone and cylinder are among the simplest and most useful new shapes.  Also available is a set of axis arrows that can be drawn anytime "
        +  "that you want to check where and how long your current coordinate axes are.  Draw it in a neutral color with the \"rgb.jpg\" "
        +  "texture image and the axes will become identifiable by color - XYZ maps to red, green, blue.<br>"
        +  "Most of these shapes are made using tiny code due to the help of two classes:  Grid_Patch and Surface_Of_Revolution (a special case "
        +  "of Grid_Patch).  Grid_Patch works by generating a tesselation of triangles arranged in rows and columns, and produes a deformed grid "
        +  "by doing user-defined steps to reach the next row or column. <br>"
        +  "All of these shapes are generated as a single vertex array each.  Building them that way, even with shapes like the axis arrows that "
        +  "are compounded together out of many shapes, speeds up your graphics program considerably.";
    }
}