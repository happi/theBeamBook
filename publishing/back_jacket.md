---
geometry: "paperwidth=7.5in,paperheight=9.25in,margin=0.75in"
fontsize: 11pt
pagestyle: empty
mainfont: "Lato"
header-includes:
  - \usepackage{xcolor}
  - \color{white}
  - \usepackage{graphicx}
  - \usepackage{eso-pic}
  - \AddToShipoutPictureBG{\AtPageLowerLeft{\includegraphics[width=\paperwidth,height=\paperheight]{publishing/bg_texture.jpg}}}
---

\vspace*{1.5cm}

\noindent
\begin{minipage}[t]{1.3in}
\vspace{0pt}
\includegraphics[width=1.3in]{publishing/erik_cropped.jpg}
\end{minipage}
\hfill
\begin{minipage}[t]{2.8in}
\vspace{0pt}
\begingroup\small
\textbf{Erik Stenman} is a software engineer, researcher, and entrepreneur with
deep expertise in Erlang, BEAM, and high-performance distributed systems.
As one of the early pioneers in BEAM internals, Erik has spent decades
optimizing concurrent systems and designing fault-tolerant architectures.
\endgroup
\end{minipage}

\vspace{0.3cm}
\noindent\textcolor{white!40}{\rule{\linewidth}{0.4pt}}
\vspace{0.6cm}

BEAM is the heart of Erlang and Elixir, powering fault-tolerant, concurrent,
and distributed systems used in finance, telecom, and messaging platforms
worldwide.

\vspace{0.3cm}

This second edition covers OTP 24 through 29, including the BeamAsm JIT
compiler that replaced the interpreter as the default execution engine.
It takes you deep into the Erlang RunTime System (ERTS), from native code
generation to process scheduling, memory management, and operational tooling.

\vspace{0.3cm}

- BeamAsm JIT compiler: architecture, code generation, and type-guided optimizations
- Process scheduling: task stealing, dirty schedulers, and priority tuning
- Memory management: garbage collection, destructive update, and carrier migration
- Tracing and profiling: trace sessions, tprof, and native coverage
- BEAM loader, compiler passes, and bytecode execution

\vspace{0.6cm}

\begingroup\small\noindent
The HappiHacking Systems Series focuses on scalable architecture, runtime
behavior, and ownership-driven system design in high-complexity environments.
\endgroup

\vfill

\noindent
\begin{minipage}[b]{2.5in}
\begingroup\scriptsize
Published by HappiHacking, Stockholm, Sweden\\
\texttt{happihacking.com}
\endgroup
\end{minipage}
