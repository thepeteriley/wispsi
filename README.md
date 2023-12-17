This is a "branched" version of the PSP/WISPR data prep, analysis and visualisation IDL routines. Initially, I'm just putting in routines that I've modified from their originals as a way to keep development separate. At some point, however, I'll add any missing routines to create 
a complete repo, or, submit these routines to the WISPR team for inclusion with their distro. Since there is currently at least one official SSW port to a GitHub repo, I could also wait for that to become publically available and submit the proposed changes there. 

With that preamble, if you're interested in using any of these routines, you'll first need to install SSW: 

https://www.mssl.ucl.ac.uk/surf/sswdoc/solarsoft/ssw_install_howto.html

If you're on a Mac, you can use Peter Young's nice guide: 

https://www.pyoung.org/quick_guides/ssw_install.html

Or, if you have Windows, you can help him complete this guide(!): 

https://pyoung.org/quick_guides/ssw_install_windows.html

Once installed and running/tested, you just need to clone WISPSI and make it visible in your IDL path. It should be the last addition to the path and should be prepended before all other directories so that these routines are 'the chosen' ones. Something like this in your startup.pro file should work: 

!path   = Expand_Path('+$IDL_STUFF/lib/wispsi') + ':' + !path

where "IDL_STUFF" is an environment variable, or complete path, to where you store your IDL files. 

To verify that you're picking up the new routines, try this from the SSW IDL prompt: 

PRINT,ROUTINE_FILEPATH("wispr_com_frame",/EITHER)

If you get an empty string returned, you may first need to compile the routine: 

.r wispr_com_frame
