---
geometry: "paperwidth=6in,paperheight=9in,margin=0in"
pagestyle: empty
header-includes:
  - \usepackage{graphicx}
  - \usepackage{xcolor}
  - \usepackage{fontspec}
  - \setmainfont[Path=/usr/share/fonts/truetype/ubuntu/,UprightFont=Ubuntu-R.ttf,BoldFont=Ubuntu-B.ttf]{Ubuntu}
  - \usepackage{tikz}
  - \usepackage{shadowtext}
  - \shadowoffset{0.7pt}
  - \shadowcolor{black!50}
  - \usepackage{eso-pic}
  - \AddToShipoutPictureBG*{\includegraphics[width=\paperwidth,height=\paperheight]{resources/redcover.jpg}}
---

\begin{tikzpicture}[remember picture, overlay]
% Title
\node at ([yshift=-1.3in]current page.north)
  {\fontsize{40}{48}\selectfont\color{white}\shadowtext{The}};
\node at ([yshift=-2.5in]current page.north)
  {\fontsize{72}{84}\selectfont\bfseries\color{white}\shadowtext{BEAM}};
\node at ([yshift=-3.5in]current page.north)
  {\fontsize{52}{62}\selectfont\bfseries\color{white}\shadowtext{Book}};
% Rule
\draw[white, opacity=0.4, line width=0.5pt]
  ([yshift=-4.1in, xshift=-1.5in]current page.north) --
  ([yshift=-4.1in, xshift=1.5in]current page.north);
% Subtitle
\node at ([yshift=-4.7in]current page.north)
  {\fontsize{15}{22}\selectfont\color{white}\shadowtext{Understanding the Erlang}};
\node at ([yshift=-5.05in]current page.north)
  {\fontsize{15}{22}\selectfont\color{white}\shadowtext{Runtime System}};
% Author
\node at ([yshift=1.0in]current page.south)
  {\fontsize{31}{36}\selectfont\bfseries\color{white}\shadowtext{Erik Stenman}};
% Publisher mark
\node[opacity=0.45, anchor=south east]
  at ([xshift=-0.3in, yshift=0.3in]current page.south east)
  {\includegraphics[height=0.3in]{resources/hh_publisher_mark.png}};
\end{tikzpicture}
