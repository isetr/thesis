# Thesis

My thesis for ELTE IK (2018)

## Machine Learning on videogames

## Description (short, English)

In this application we are using Reinforcement Learning on a simulation of a video game: Final Fantasy XIV.
The project is split into two parts of this problem: Simulating the fight system of the game, Machine Learning on said system.

The goal is to achieve an optimal sequence of skills in the given fight system which results in a high overall damage output.

The Machine Learning algorithm used here is a Monte Carlo algorithm. 

## Description (Hungarian)

A program célja a gépi tanulás (machine learning – ML) alkalmazásának bemutatása, videójátékokon. 
Ennek a megvalósításához egy-egy játék mechanizmusát le kell modellezni a hatékonyság érdekében (például az idő sokszoros gyorsítása, vagy akár a grafikus felület figyelmen kívül hagyása, egyszerűsítése), illetve ennek segítségével a környezet felett teljes uralmat kaphatunk különböző esetek szimulálásához is.

A program egy játék ilyen modelljén keresztül kapja a feladatot, hogy egy ML bemutatására alkalmas tevékenységet tanuljon meg, és ennek a tevékenységnek egy maximális hatékonyságú változatát meg is találja.

Egy példa a Final Fantasy XIV című játék harc rendszere. 
Ebben a rendszerben a cél egy adott hős képességeivel a maximális DPS (damage per second) elérése egy adott idő intervallumban. A képességek használatával a játékos sebzést (damage) okoz, viszont utána egy ideig nem használhat másik képességet.

A programnak feladata megtalálni a képességek optimális kombinációját, illetve ezen képességek használatának megfelelő időzítését. A képességek több időzítőtől is függhetnek, illetve egy-egy képesség hatással lehet az utána következő egy, vagy több értékre is. 

Egyes képességek nem csak az adott hős képességeire lehetnek hatással, hanem segíthetik a vele együtt lévő hősökét is. Ebből adódóan lehetőség nyílik nem csak egyéni tanulásra, hanem négy, illetve nyolc fős csoportok tanítására is, ahol már nem csak az egyéni DPS fontos, hanem a csoport teljes DPS értéke.
