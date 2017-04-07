# Hi

Hi I opened this repo to the public today (April 7) to share it with some students. I was giving a lecture about the BEAM at Chalmers. I didn't think anyone else would notice. I was not planning to release this for real yet since the repo currently just contains bits and pieces from several different versions of the book that I have been writing.

I hope to bring in more chapters and fill out the existing chapters in the weeks to come, but please feel free to comment and correct any errors you find. The plan is to make this into a collaborate effort so that we can get a complete documentation of the Erlang Runtime system as soon as possible.

# About this book
A description of the Erlang Runtime System ERTS and the virtual Machine BEAM.

This is an attempt to document how the internals of the Erlang Runtime System work.

Please feel free to contribute since this work is far from done, and it will probably never be done since there is continuous development of the Erlang runtime system.

The book is written in AsciiDoc and can be read directly on github in your browser or you can create a pdf from the source code.

To read the book online just open the file [book.asciidoc](book.asciidoc).


## Building

To build a pdf from source you will need make, asciidoc, pdflatex.


 sudo apt-get install asciidoc
 make
 
 # TODO
 * Bring in missing chapters
 * Bring in missing text in the process and scheduler chapters
 * Fix the mess caused by mergin two different versions into the first chapters
 * Go over the new outline
 * Fix links
 * Add images
 * Fix build system for ascii-art
 * Bring in the beam instruction documentation automatically from the latest Erlang release 
 * Set up a build system that produces a new pdf on check-in
 * Links to OTP code on github
 * Add some kind of forum/wiki to discuss what needs to be done
 * Index
