(* ::Package:: *)

(*This script copies the DiffusiveDynmaics subdirectory into $UserBaseDirectory/Applications. 
  If the directory with the same name allready exists a backup copy is made just in case, 
   and then the directory is deleted.*)

SetDirectory@NotebookDirectory[];

(*Get the target dir*)
targetDir=FileNameJoin[{$UserBaseDirectory,"Applications","DiffusiveDynamics"}];

(*Backup dir if it exists just in case*)
Quiet@CopyDirectory[targetDir,
     FileNameJoin[{$UserBaseDirectory,"Applications",
                   "DiffusiveDynamics-Backup"<>DateString[{"Year","Month","Day"}]}]];
(*Delete the directory*)
DeleteDirectory[targetDir,DeleteContents->True];
result=CopyDirectory["DiffusiveDynamics",targetDir];
If[result=!=$Failed,Needs["DiffusiveDynamics`"];"Successfully installed package to:\n"<>targetDir]
