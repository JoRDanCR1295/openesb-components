=============================================================

At the prompt ">", type a command letter and  the required and optional parameters, always use quotes
around the parameter value. Optionally, you can write the output of a command to
a specific file in addition to the console.

Type command: 'h' or 'help' shows all commands 

e.g

1. Showing the information of all bpel instances :

>b  <RET>

2. Showing the information of all running instances:

>b status="running" <RET>

3. Showing the information of all running and suspended instances:

>b status="running|suspended" <RET>

4. Writing the output to the file  given full path of the  file, overwriting
the file if  file exists

>b status="running" >"c:\work\output.txt" <RET>

5. Writing the output to the file  given full path of the  file, appending to
the file if file exists

>b status="running" >"c:\work\output.txt"+ <RET>

=============================================================