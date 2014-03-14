{-# LANGUAGE FlexibleInstances #-}
module Printing where
    
import Data.List
import Text.PrettyPrint    
import Types
import Text.Printf    
    
showable :: (Show a) => a -> Doc    
showable = text . show
    
class Printable a where
    pp :: a -> Doc
    
instance Printable MatchReport where
    pp (MatchReport ps) = text "\\begin{tabular}{| l | l |" <> text (concat $ replicate (length . metricNames $ ps) " l |") <> text "}"
        $+$ nest 4 (text "\\hline")
        $+$ nest 4 ((foldr (<>) empty . map (\name -> text "&" <> text name) . metricNames $ ps) <> text "\\\\")
        $+$ nest 4 (text "\\hline")
        $+$ nest 4 (foldr ($+$) empty . map ppProject $ ps)
        $+$ text "\\end{tabular}"
        where
          ppMetric (MatchMetric name number) = text "&" $+$ showable number
          ppMetric (UnfinishedMetric name)   = text "&" $+$ text "d.n.f."

          ppProject (Project name metrics) = text "\\textbf{" <> text name <> text "}"
                                         $+$ (foldr ($+$) empty . map ppMetric . makeRelative $ metrics)
                                         $+$ text "\\\\"
                                         $+$ text "\\hline" 


instance Printable GCReport where
    pp (GCReport xs) = text "\\begin{tabular}{| l | l | l |}" 
                    $+$ nest 4 (text "\\hline")
                    $+$ nest 4 (text " & GC Enabled (s) & GC Disabled (s) \\\\")
                    $+$ nest 4 (text "\\hline")
                    $+$ nest 4 (foldr ($+$) empty . map ppProject $ xs)
                    $+$ text "\\end{tabular}"
                    where
                        ppProject (Project name [enabled, disabled]) = text "\\textbf{" <> text name <> text "}"
                                                                   $+$ text "&"
                                                                   $+$ (text . printf "%.2f" . executionTime $ enabled)
                                                                   $+$ text "&"
                                                                   $+$ (text . printf "%.2f" . executionTime $ disabled)
                                                                   $+$ text "\\\\"
                                                                   $+$ text "\\hline"


instance Printable [Project] where
    pp ps = text "\\begin{tabular}{| l | l |" <> text (concat $ replicate (length . metricNames $ ps) " l |") <> text "}"
        $+$ nest 4 (text "\\hline")
        $+$ nest 4 (text "&" <> (foldr (<>) empty . map (\name -> text "&" <> text name) . metricNames $ ps) <> text "\\\\")
        $+$ nest 4 (text "\\hline")
        $+$ nest 4 (foldr ($+$) empty . map pp $ ps)
        $+$ text "\\end{tabular}"
        
instance Printable Project where
    pp (Project name metrics) = text "\\parbox[t]{2mm}{\\multirow{5}{*}{\\rotatebox[origin=c]{90}{\\textbf{" 
                             <> text name 
                             <> text "}}}}" 
                            <+> printMetrics metrics
                            $+$ text "\\hline"

                            
printMetrics :: [Metric] -> Doc
printMetrics ms = foldr (<>) empty 
                . concat 
                . map (\(count,row) -> row ++ [text "\\\\\n"] ++ if count == 4 then [text "\\cline{3-" <> text (show $ length ms + 2) <> text "}"] else [])
                . zip [1..]
                . map (\(label,row) -> (text "&" <+> label):row)
                . zip [ text "\\# of union types"
                      , text "\\# of union types coll."
                      , text "\\# of poly. call sites"
                      , text "\\# of callgraph edges"
                      , text "\\# of context elements"
                      , text "\\# of iterations"
                      , text "\\# of precise matches"
                      , text "average var points-to"
                      , text "execution time (s)"
                      ]
                . map (map (\doc -> text "&" <+> doc)) 
                . transpose 
                . map printMetric
                . makeRelative
                $ ms
                where 
                    printMetric m@(FinishedMetric _ _ _ _ _ _ _ _ _ _) = [ showable $ numberOfUnionTypesWithoutCollapsing m
                                                                   , showable $ numberOfUnionTypesWithCollapsing m
                                                                   , showable $ numberOfPolymorphicCallSites m
                                                                   , showable $ numberOfCallGraphEdges m
                                                                   , showable $ numberOfContexts m
                                                                   , showable $ numberOfIterations m
                                                                   , showable $ numberOfPreciseMatches m
                                                                   , text . printf "%.2f" . averageVarPointsTo $ m
                                                                   , text . printf "%.2f" . executionTime $ m
                                                                   ] 
                    printMetric (UnfinishedMetric _)           = replicate 8 (text "d.n.f.")
   
    
--  \begin{tabular}{| l | l | l | l | l | l | l |}
--    \hline
--                                                                           &                & Insensitive & 1obj & 1obj+1H & 2plain+1H & 2full+1H \\
--    \hline
--    \parbox[t]{2mm}{\multirow{2}{*}{\rotatebox[origin=c]{90}{Ray Tracer}}} & Singleton type & 392         & 475  & 475     & 481       & 493      \\
--                                                                           & Exec time      & ?           & ?    & ?       & ?         & ?        \\  
--    \hline                                                                         
--    \parbox[t]{2mm}{\multirow{2}{*}{\rotatebox[origin=c]{90}{PHP Geo}}}    & Singleton type & ?           & ?    & ?       & ?         & ?        \\
--                                                                           & Exec time      & ?           & ?    & ?       & ?         & ?        \\
--    \hline
--    \parbox[t]{2mm}{\multirow{2}{*}{\rotatebox[origin=c]{90}{Gaufrette}}}  & Singleton type & ?           & ?    & ?       & ?         & ?        \\
--                                                                           & Exec time      & ?           & ?    & ?       & ?         & ?        \\
--    \hline    
--    \label{plainvsfull}                                                                     
--  \end{tabular}
--
--
--                (\m -> [ showable $ numberOfSingletonTypes m
--                             , showable $ numberOfPolymorphicCallSites m
--                             , showable $ numberOfCallGraphEdges m
--                             , text . printf "%.2f" . averageVarPointsTo $ m
--                             , text . printf "%.2f" . executionTime $ m
--                             ]) 