<%-- 
    Document   : entry
    Created on : Oct 31, 2007, 6:34:38 PM
    Author     : radval
--%>

<%@page contentType="text/html" pageEncoding="windows-1252"%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
   "http://www.w3.org/TR/html4/loose.dtd">

<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=windows-1252">
        <title>Main Console</title>
         <!-- Required CSS -->
        <link type="text/css" rel="stylesheet" href="http://yui.yahooapis.com/2.3.1/build/treeview/assets/skins/sam/treeview.css">
 
         <!-- Dependency source files --> 
        <script src = "http://yui.yahooapis.com/2.3.1/build/yahoo/yahoo-min.js" ></script>
        <script src = "http://yui.yahooapis.com/2.3.1/build/event/event-min.js" ></script>

        <!-- TreeView source file --> 
        <script src = "http://yui.yahooapis.com/2.3.1/build/treeview/treeview-min.js" ></script>
        
    </head>
    <body>
        <h2>Main Console</h2>
        <div id="treeDiv1" />
        <script type="text/javascript">
            var tree;
            function treeInit() {
                    alert("test");
                    tree = new YAHOO.widget.TreeView("treeDiv1");
                    var root = tree.getRoot();
                    var tmpNode = new YAHOO.widget.TextNode("mylabel", root, false);
                    var tmpNode2 = new YAHOO.widget.TextNode("mylabel1-1", tmpNode, false);  
                     
                    tree.draw();
            }

            window.open = treeInit();
        </script> 
        
    </body>
</html>
