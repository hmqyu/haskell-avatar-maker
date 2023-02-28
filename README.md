# a simple avatar maker 👤

## table of contents

* [summary](#summary)
* [setup](#setup)
* [walkthrough](#walkthrough)

## summary

a simple avatar maker is a haskell project that was built for cpsc312 project 1. it's a command-line app that prompts the user to design an avatar, given various questions and answers. after designing, the program will take the selected assets and transform them as needed to produce a .PNG file of the avatar. the file is uploaded to the designated `output` folder and can then be used as your profile picture!

## setup

### prerequisites 

1) [`ghcup`](https://www.haskell.org/ghcup/) must be installed on your device.
2) [`ghc`]() [`cabal-install`]() and [`stack`]() must also be installed on your device. they can be installed during the installation of [`ghcup`](https://www.haskell.org/ghcup/)

clone or download/unzip the repository and open it in a command-line interface. run the following command to download the project dependencies:

```
stack setup
```

run the following commands to build & play the project via command-line:

```
stack build
stack run
```

## walkthrough

1. the app will prompt you for an answer using different questions and possible answers, in order to build your avatar:

    <img src="https://i.imgur.com/5ofnTHT.png">

2. after completing all the questions, the program will build your avatar:

    <img src="https://i.imgur.com/tGvC6GT.png">

...which can be found in the output folder of the project:

    <img src="https://i.imgur.com/pXWunOW.png">

    <img src="https://i.imgur.com/vmP00rm.png">

3. the program will ask if you'd like to create another avatar or not, where you can continue playing or quit:

    <img src="https://i.imgur.com/TXcG5Ts.png">

## stack
- haskell
- stack
- juicypixels
