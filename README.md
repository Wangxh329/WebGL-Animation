# WebGL-Animation

### Xiaohan Wang
### CS 174A, Fall 2017


# Instructions
This is the second project of CS 174A in UCLA. To upload the picture texture and background music successfully, you cannot only open it with Chrome since 
it cannot upload some materials in the folder. You need to:
  - Method 1: open with Firefox(easy, recommend);
  - Method 2: add the project to a server. Open your terminal, and go to the directory where index.html locates, and then input ``python -m http.server`` or ``python -m SimpleHTTPServer``. Then open your browser and type in URL: <http://localhost:8000>


# Story & Animation Design
### Summary
I was inspired by a Chinese idiom --- “Three people spreading reports of a tiger make you believe there is one around”, in literal translation. In the small story, there is a crocodile bending aside the river. One passing-by buffalo saw it and mistook it as a wood which can help them to cross the river. Then came three buffalos, thought they thought at first was a crocodile, they all believed it's a wood after the tentative feel-out. Finally came a group of buffalos, after hearing so many buffalos saying it's a wood, they took it without thinking. The leader then prepared to cross the river, however the crocodile suddenly attacked it and killed it. It comes a long silence... From the story, I'd like to express that a repeated slander makes other believe. A wise man is supposed to distinguish what to believe or not, and not to be a talebearer.

Role: 1 crocodile, 7 buffalos

### Animation Design
- 0-5s：buffalo_1 is walking towards river side(camera tracking)

- 5-8s：buffalo_1 saw a float black thing aside, surprised(camera change)

- 8-11s：buffalo_1 guessed it’s a wood(camera change)

- 11-17s: buffalo_1 touched the “wood” and confirmed it’s a wood(camera tracking)

- 17-20s: buffalo_2 also came to the river side(camera change)

- 20-22s: buffalo_1 told buffalo_2 that it’s a wood(camera change)

- 22-24s: buffalo_2 thought first was a dangerous crocodile

- 24-32s: buffalo_1 jumped onto the “wood”. It seemed safety, and buffalo_2 now believed it’s a “wood”

- 32-37s：buffalo_3 and buffalo_4 also came here(camera change)

- 37-40s: buffalo_1 and buffalo_2 thought it was a wood, but buffalo_3 and buffalo_4 thought was a crocodile(camera change)

- 40-48s: buffalo_2 jumped onto the “wood” to prove safety

- 48-56s: buffalo_3 throw a stone to the black one, but with no response, so they all believed it’s a wood(camera change, focus on the stone)

- 56-66s: three new buffalos came here(camera change)

- 66-70s: new three buffalos believed it’s a wood because the four guys there all said so(camera change)

- 70-75s: the leader of the three new buffalos walk toward the “wood” to cross the river(camera change)

- 75-85s: crocodile swim to the leader and then ate it suddenly(camera change)

- 85-90s: all silent. Then all change their thought to opposite immediately…(camera change)

- 90-93s: THE END


# Grading Points
- [4 points] Creativity (story, aesthetic style, etc).
  
  See above the story and animation design.

- [4 points] Complexity and impressive underlying mechanics.

  See above the story and animation design.

- [5 points] Overall quality: Fluidity of object and camera motion, attention to detail in scene construction, etc.

  See above the story and animation design.

- [4 points] Make use of hierarchical objects with at least three levels (e.g., a human arm)

  ``draw_buffalo()``, ``draw_crocodile()``, etc use the hierarchical objects. For example, ``cylinder`` & ``cone`` --> ``draw_horn()`` --> ``draw_head()`` --> ``draw_buffalo()`` 

- [4 points] Demonstrate the camera tracking a moving object by overwriting the camera matrix using Mat4.look_at(). Specify in your README.txt when in your animation this happens, or which input (if any) must be entered to trigger it.

  See above the story and animation design.

- [6 points] Design polygonal objects of your own to supplement the existing ones. To specify these shapes you must provide novel positions, normals, and texture coordinates to the graphics card by extending class Shape .

  New polygonal objects mainly include two categories: modify and made from surface_demo (like Half_Trapezoidal_Cylinder, Cylinder, Gradient_Tube, etc), and made from scratch(like LeafShape, Semi_Sphere, etc)

- [2 points] Assign reasonable texture coordinates to, and texture, an instance of your custom polygonal object. Either texture it by mapping an image file, or procedurally without an image.

  Texture mainly by mapping an image file, also include some RGBA color in OpenGL

- [2 points] Real-time speed. Make sure that your animation runs at the same speed i.e., one simulated second corresponds roughly to one real second regardless of the machine your program runs on (even a ridiculously fast one from the future). The variable animation_time inside the graphics_state object is your gauge of the passage of real seconds.

  Use with animation_time to update Shapes in each scene

- [2 points] Display the frame rate of your program somewhere, probably by taking advantage of the live_string() function, mimicking how it is used in some subclasses of Scene_Component.

  As displayed in left chart below the animation, i.e. make_control_panel()

- [TBA points] Be on the lookout for a possible announcement about more extra credit you could earn by integrating certain physics-based objects somewhere in your animation.

  I integrated one physical rule when buffalo jumped from river side to the back of crocodile:
  	When delta_t = 0, buffalo1_y = 1;
    When delta_t = 2, buffalo1_y = -1;
    In direction x & z, just linearly change the position. But in direction y, it has gravity, then let largest height be h, time from 1 to h is t1, time from h to -1 is t2, then

      h = 1 + 1/2 x g x t1^2      

      -1 = h - 1 / 2 x g x t^2  

      t1 + t2 = 2             

  --> h = 5.05, t1 = 0.9, t2 = 1.1

    Then we can get the parabolic function is y = -5 x delta_t^2 + 9 x delta_t+1
