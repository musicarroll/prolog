
:-module('texgen',
        [
		string_insert/4,
        tex_def/2,
        tex_fig/4,
        write_tex_fig/3,
        write_article_back/0,
        write_article_front/0
        ]).
:-use_module('functions.pro').

% See:  interpolate_string(:In, -Out, +Map, +Options)
string_insert(Insert,Front,End, Result) :- 
    string_concat(Front,Insert,NewFront),
    string_concat(NewFront,End,Result).

tex_def(Internal,Def):- string_insert(Internal, 
                    '\\begin{definition}\\label{def:TBD}\n','\n\\end{definition}',
                    Def).

tex_fig(Fname,Caption,Label, Figure):- 
    string_insert(Fname,
        '\\begin{figure}\n\t\\includegraphics[scale = 1.0]{','}\n\t',
        Front),
    string_insert(Caption, '\\caption{','}\n\t',Middle),
    string_insert(Label,'\\label{fig:','}\n\\end{figure}',End),
    string_concat(Front, Middle,Tmp),
    string_concat(Tmp, End, Figure).  

write_tex_fig(Fname,Caption,Label):-
    format(
'\\begin{figure}
    \\includegraphics[scale = 1.0]{~w}
    \\caption{~w}
    \\label{fig:~w}
\\end{figure}
',[Fname,Caption,Label]).

write_article_front:-
    format(
'\\documentclass{article} 
\\usepackage{amsmath}  
\\usepackage{graphicx}  

\\begin{document}  
').

write_article_back:-
    format('
\\end{document}
').

seed_set_automorph_to_latex(SSet,VSet):- 
