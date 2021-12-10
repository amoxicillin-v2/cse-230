# cse-230

## Intro

A maze game

The game will allow the user to walk through the maze (the user can only see part of the map while walking) to defeat monsters, avoid traps, find treasure and finally reach the target in the maze


### UI Design

```python

// ---- CHARACTER ----
// HP: 9
// MOV: 0
// FOV: 2
// ---- --------- ----

 ---- ---- ---- ---- ----
|CHAR|    | X  | ?  | ?  |
 ---- ---- ---- ---- ----
|    |    | X  | ?  | ?  |
 ---- ---- ---- ---- ----
| X  |MOST| X  | ?  | ?  |
 ---- ---- ---- ---- ----
| ?  | ?  | ?  | ?  | ?  |
 ---- ---- ---- ---- ----
| ?  | ?  | ?  | ?  |GOAL|
 ---- ---- ---- ---- ----

w/s/a/d> 
```

### Library used

In this project we used several libraries:

Vty: Graphic library used to print out the proper graph

Brick: TUI library, used to build a proper user interface

Containers: Used as a container to contain information.


## Architecture

![](https://raw.githubusercontent.com/amoxicillin-v2/cse-230/dev-yiz/doc/SimpleMaze.drawio.svg)

Our project will have about 9 key components: Menu, GameLevel, LevelGen, Character, iBlock, Config, Input, Printer and Logger.

Menu is the component which will allow the users to begin the game.

GameLevel component will allow the users to select the level of the game (for example, easy, medium or hard)

LevelGen component should generate a random maze according to the game level selected by users.

Character component represents the game character controlled by the users, it will have health, position and fov these kind of information, and it can move

iBlock components are the blocks in the maze, some of them may be empty, some of them may have monster within, and there will always be one goal block in the maze, once the users reach it, they win.

Config component will try to read the whole structure of the maze from a file.

Input component is responsible for reading the users' input and passing information to other components.

Printer component will try to print necessary information to the users, such as the state of the game character, the visible maze map.

Logger component will store the logs of our project.

## Challenges

Currently we have three main challenges in our project.

First, how should we implement the Config component so that it can load a maze from a file? We plan to use Json structure to solve it, and we are still searching for a proper library of Haskell.

Second, what kind of information should we print so that the users could have a great game experience? After discussion, we plan to only print 3x3 blocks, which centered at the game character's location, so that the users won't get a full map of the maze, just like a real maze, you can only see the blocks around you.

Third, we should make this maze alive, if possible we want to add some animation to the components of our maze.

The last challenge is how to implement the LevelGen component, it is not easy to randomly generate a valid maze, if you just randomly set the walls and blocks, the users may get an invalid maze which they can never find a way to the goal. For this challenge we are still trying to find a proper solution.

## Plan

We think we can finish most of the components in our project and make it work well, and if time is limited, we may remove the LevelGen and Logger components in our project, because we can predetermine the structure of the maze rather than truly generate a random maze. And the Logger component won't affect our project.



## Collaborators

Chao-Te Chou

Yeyang Zhou

Yichi Zhang

Zhiyuan Xiang
