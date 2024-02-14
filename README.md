# chess

I developed this Chess code in 2018 as my final project for a lecture during my undergraduate studies at ITU, with the help of [Dr. Efe](https://www.researchgate.net/profile/Bahtiyar-Efe "BahtiyarEfe").
  
This Fortran code can be easily improved, and I’m eager to share it with anyone interested in learning this programming language or looking to practice by reviewing the code.

    
__Suggestions for improving the code:__
1. The special chess movements, such as castling, can be adapted.
2. Consider simplifying the movement of pieces. Instead of specifying locations (for example, from [7,5] to [5,5]), you could enhance the code to allow movements with simpler inputs like [e4].
  
  
> __To play the game:__  
> $ gfortran -o chess.v1 code.f95  
> $ ./chess.v1  
  
Here is the Ruy Lopez opening...  
![ruylopez](https://github.com/cansud98/chess/assets/81981060/4bf945f8-6ae1-49b8-a770-9ab35123bc37)

"The Ruy Lopez—also known as the Spanish game—is named after Rodrigo (Ruy) Lopez de Segura, a Spanish bishop who analyzed this opening in his 1561 work, "Book of the Liberal Invention and Art of the Game of Chess". Nearly half a millennium later, the Ruy remains one of the most popular chess openings. Chess experts have come up with numerous variations, and a wide variety of strategic plans are available to both white and black.  
  
The starting position of the Ruy Lopez is reached after the following moves: 1. e4, e5; 2. Nf3, Nc6; and 3. Bb5.  
  
Popular lines in the Ruy Lopez include but are not limited to the Morphy defense, Steinitz defense, and the Berlin defense. Each of these and several other variations lead to numerous sub-variations."  
*Source: [Edward Scimia](https://www.thesprucecrafts.com/most-common-chess-openings-611517 "website")*
