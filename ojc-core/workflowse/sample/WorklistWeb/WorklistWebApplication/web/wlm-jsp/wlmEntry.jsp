<%@ page import = "java.net.*" %>
<html>
    <head>
        <link rel="stylesheet" type="text/css" href="../yui_2_5_2/assets/yui.css">

        <!-- YAHOO Global Object source file -->
        <script type="text/javascript" src="../yui_2_5_2/yahoo/yahoo-min.js">
        </script><!-- DataSource -->
        <!-- Dependencies -->
        <script type="text/javascript" src="../yui_2_5_2/event/event-min.js">
        </script><!-- OPTIONAL: Connection (enables XHR) -->
        <script type="text/javascript" src=
                "../yui_2_5_2/connection/connection-min.js">
        </script><!-- Source file -->
        <script type="text/javascript" src=
                "../yui_2_5_2/datasource/datasource-beta-min.js">
        </script><!-- DataTable -->
        <!--CSS file (default YUI Sam Skin) -->
        <link type="text/css" rel="stylesheet" href=
              "../yui_2_5_2/datatable/assets/skins/sam/datatable.css">
        <!-- Dependencies -->
        <script type="text/javascript" src=
                "../yui_2_5_2/yahoo-dom-event/yahoo-dom-event.js">
        </script>
        <script type="text/javascript" src=
                "../yui_2_5_2/element/element-beta-min.js">
        </script>
        <script type="text/javascript" src=
                "../yui_2_5_2/datasource/datasource-beta-min.js">
        </script><!-- OPTIONAL: JSON Utility -->
        <script type="text/javascript" src="../yui_2_5_2/json/json-min.js">
        </script>
        <!-- OPTIONAL: Drag Drop (enables resizeable or reorderable columns) -->
        <script type="text/javascript" src=
                "../yui_2_5_2/dragdrop/dragdrop-min.js">
        </script><!-- OPTIONAL: Calendar (enables calendar editors) -->
        <script type="text/javascript" src=
                "../yui_2_5_2/calendar/calendar-min.js">
        </script><!-- Source files -->
        <script type="text/javascript" src=
                "../yui_2_5_2/datatable/datatable-beta-min.js">
        </script><!-- Logger --><!--CSS file (default YUI Sam Skin) -->
        <link type="text/css" rel="stylesheet" href=
              "../yui_2_5_2/logger/assets/skins/sam/logger.css">
        <!-- Source file -->
        <script type="text/javascript" src=
                "../yui_2_5_2/logger/logger-min.js">
        </script><!-- Container -->

        <!-- OPTIONAL: Animation (only required if using ContainerEffect) -->
        <script type="text/javascript" src=
                "../yui_2_5_2/animation/animation-min.js">
        </script>
        <!-- OPTIONAL: Connection (only required if using Dialog/SimpleDialog) -->
        <script type="text/javascript" src=
                "../yui_2_5_2/connection/connection-min.js">
        </script>
        <!-- OPTIONAL: Drag & Drop (only required if enabling Drag & Drop for Panel/Dialog/SimpleDialog) -->
        <script type="text/javascript" src=
                "../yui_2_5_2/dragdrop/dragdrop-min.js">
        </script>
        <!-- OPTIONAL: YUI Button (only required if using Dialog/SimpleDialog with YUI Buttons) -->
        <script type="text/javascript" src=
                "../yui_2_5_2/button/button-min.js">
        </script><!-- Source file -->
        <script type="text/javascript" src=
                "../yui_2_5_2/container/container-min.js">
        </script><!-- Sam Skin CSS -->
        <link rel="stylesheet" type="text/css" href=
              "../yui_2_5_2/container/assets/skins/sam/container.css">
        <!-- Core + Skin CSS -->
        <link rel="stylesheet" type="text/css" href=
              "../yui_2_5_2/button/assets/skins/sam/button.css">
        <!-- OPTIONAL: Menu Stylesheet (required for creating buttons of type "menu" and "split") -->
        <link rel="stylesheet" type="text/css" href=
              "../yui_2_5_2/menu/assets/skins/sam/menu.css">
        <script type="text/javascript" src="../yui_2_5_2/menu/menu-min.js">
        </script><!-- Source file -->
        <script type="text/javascript" src=
                "../yui_2_5_2/button/button-min.js">
        </script>
        <script type="text/javascript" src="../yui_2_5_2/utilities/utilities.js"></script>
        <link rel="stylesheet" type="text/css" href="../yui_2_5_2/reset-fonts-grids/reset-fonts-grids.css">
        <link rel="stylesheet" type="text/css" href="../yui_2_5_2/assets/skins/sam/skin.css">
        <script type="text/javascript">

            <!-- User Configurable Begin -->
            var refreshInterval = 10000; //default refreshing = 10 sec
            var pageSize = 20; //Pagination Enabled, tasks per page, 0 means no pagination
            var savedSort = "id";
            var savedDir = "ASC";
            <!-- User Configurable End -->

            var pageStart = 0;
            var currentPageRecords = 0;
            var inRefresh = false;
            var selectedRecord = null;
            var response = null;
            var totalRecords = 0;
            var oldPageRecords = 0;
            var taskDetailsURL = null;
            var toolBox = null;
            var reAssignToolBox = null;
            var searchText = "";
            var searchStatues = "";
            var searchUsers = "";
            var searchGroups = "";
            var reAssignUser = "";
            var reAssignGroup = "";
            var searchType = 'DEFAULT';


        </script>

        <%
        String remoteUser = request.getRemoteUser();
        %>

        <script type="text/javascript">

            YAHOO.namespace("example.container");

            var taskListURL = window.location.href;
            taskListURL = taskListURL.replace('wlmEntry.jsp', "getTaskList.jsp?");

            function refreshTaskList() {
                if (inRefresh)
                    return;
                inRefresh = true;

                var oCallback = { success : dataReturned,
                    failure : dataReturned,
                    scope : this
                };

                if(taskListURL != null) {
                    myDataSource.sendRequest("start=" + pageStart +
                        "&pageSize=" + pageSize + "&sort=" + savedSort + "&dir="+ savedDir + "&searchStatues=" + searchStatues +
                        "&searchUsers=" + searchUsers + "&searchGroups=" + searchGroups + "&searchString="+searchText + "&searchType=" + searchType ,
                    oCallback);
                }
            }

            function init() {
                hideIframe();
                self.setInterval("refreshTaskList()", refreshInterval);

            }


            function dataReturned (sRequest , oResponse , oPayload ) {

                myDataTable.getRecordSet().deleteRecords (pageStart, oldPageRecords);

                myDataTable.getRecordSet().addRecords(oResponse.results, pageStart);

                myPaginator.setTotalRecords(totalRecords, true);

                myDataTable.render();

                myPaginator.render ();

                response = oResponse;

                showSelectedTask(oResponse);

            }


            function showSelectedTask (oResponse) {

                myDataTable.unselectAllRows();

                var records = myDataTable.getRecordSet ();
                var length = records.getLength();
                var found = false;

                if (isWaiting) {
                    isWaiting = false;
                    hideWait();
                    //setTimeout("hideWait()", 500);
                }

                if(selectedTaskId != null && length > 0) {
                    //var start_end = myPaginator.getPageRecords();
                    if (YAHOO.lang.isArray(response.results) && response.results.length > 0) {

                        for(var i= pageStart; i <= myDataTable.getRecordSet ().getLength() && !found; i++) {
                            var record = records.getRecord(i);
                            if(record != null) {

                                var taskId = record.getData("id");

                                if(taskId != null && taskId == selectedTaskId) {


                                    var claimedBy = record.getData("owner");

                                    //checkOutButton.setStyle("visibility", "hidden");
                                    //completeButton.setStyle("visibility", "hidden");
                                    checkOutButton.set("disabled", true);
                                    completeButton.set("disabled", true);

                                    if (claimedBy == null || claimedBy == '') {
                                        checkOutButton.set("disabled", false);
                                    }

                                    if(claimedBy != null && claimedBy != '') {
                                        completeButton.set("disabled", false);
                                    }


                                    found = true;

                                    myDataTable.selectRow(myDataTable.getRow(record));
                                    selectedRecord = record;

                                    break;

                                } //if
                            }//if
                        }//for
                    }//if

                }//if

                if (!found) {
                    selectedRow = null;
                    selectedTaskId = null;
                    selectedRecord = null;
                    hideIframe();

                }

                inRefresh = false;

            }

            YAHOO.util.Event.addListener(window, "load", init);

        </script>
        <script type="text/javascript">

            /*
             * AjaxObject is a hypothetical object that encapsulates the transaction
             *     request and callback logic.
             *
             * handleSuccess( ) provides success case logic
             * handleFailure( ) provides failure case logic
             * processResult( ) displays the results of the response from both the
             * success and failure handlers
             * call( ) calling this member starts the transaction request.
             */

            var AjaxObject = {

                handleSuccessRefresh:function(o){

                    showTaskDetails(selectedTaskId, "<%=remoteUser%>", false);
                    this.processResult(o);

                },

                handleSuccessNonRefresh:function(o){
                    if(selectedRow != null && selectedRow[0] != null) {
                        selectedTaskId = null;
                        selectedRow = null;
                    }
                    this.processResult(o);
                },

                handleFailure:function(o){
                    // Failure handler
                    YAHOO.log("failed to get task details"+ o.statusText , "info");
                    var taskListActionContainer = document.getElementById('taskListActionContainer');
                    taskListActionContainer.innerHTML = o.responseText;
                    //YAHOO.example.container.wait.hide();
                },

                processResult:function(o){
                    refreshTaskList ();
                },

                startRequest:function(url, callbackVar) {
                    YAHOO.log("loading url taskDetails: "+ url , "info");
                    YAHOO.util.Connect.asyncRequest('POST', url, callbackVar, null);
                }

            };

            /*
             * Define the callback object for success and failure
             * handlers as well as object scope.
             */
            var callbackRefresh =
                {
                success:AjaxObject.handleSuccessRefresh,
                failure:AjaxObject.handleFailure,
                scope: AjaxObject
            };

            var callbackNonRefresh =
                {
                success:AjaxObject.handleSuccessNonRefresh,
                failure:AjaxObject.handleFailure,
                scope: AjaxObject
            };

            function executeTaskListAction(taskId, taskListAction) {
                var taskListActionURL ="taskListAction.jsp";
                if(taskId != null) {
                    taskListActionURL = taskListActionURL + "?taskId="+taskId;
                }

                if(taskListAction != null) {
                    taskListActionURL = taskListActionURL + "&taskListAction="+ taskListAction;
                }

                taskListActionURL = taskListActionURL + "&reAssignUser="+ reAssignUser;
                taskListActionURL = taskListActionURL + "&reAssignGroup="+ reAssignGroup;


                if (taskListAction == "complete") {
                    AjaxObject.startRequest(taskListActionURL, callbackNonRefresh);
                } else
                {
                    AjaxObject.startRequest(taskListActionURL, callbackRefresh);

                }
            }

            function showTaskDetails(taskId, claimedBy, isClicked) {
                var taskDetailsURL_new ="../xforms-jsp/taskDetails.jsp";
                if(taskId != null) {
                    taskDetailsURL_new = taskDetailsURL_new + "?taskId="+taskId;
                }


                if(claimedBy != null) {
                    taskDetailsURL_new = taskDetailsURL_new + "&claimedBy="+ claimedBy;
                }

                if (isClicked ||taskDetailsURL_new != taskDetailsURL) {
                    taskDetailsURL = taskDetailsURL_new;
                    loadIframe('taskDetailsIframe', taskDetailsURL);
                }

            }



        </script>
        <style type="text/css">
            /* custom css*/
            #welcome {
                padding-left: 520px;
                padding-top: 0px;
            }
            #welcome span {
                font-family: Verdana, Times New Roman, Arial, Times, serif;
                font-size: 23px;
                font-style: italic;
                font-weight: bolder;
                color: #76708a
            }
            #welcome p {
                font-family: Verdana, Geneva, Arial, Helvetica, sans-serif;
                font-size: 16px;
            }
            #header {
                width: 800px;
                height: 101px;
                margin: 0 auto;
                margin-bottom: 20px;
                padding: 0;
                background: url(../newBanner.jpg) no-repeat left top;
                font-family: Arial, Helvetica, sans-serif;
            }

            #header h1, #header p {
                margin: 0;
            }

            #header h1 {
                float: left;
                color: #FFFFFF;
                font-size: 30px;
                padding-left: 120px;
            }

            #header p {
                float: left;
                padding: 17px 0 0 10px;
                font-size: 12px;
                font-weight: bold;
                color: #625FA1;
            }

            #header a {
                text-decoration: none;
                color: #F79F1A;
            }

            .yui-skin-sam .yui-dt table
            {font: normal normal normal 11px Verdana, Geneva, Arial, Helvetica, sans-serif;
            }

            .yui-skin-sam .yui-dt thead {
                background: url(../dtable.gif) #FFF repeat-x scroll center left;
                border-top: 1px solid #CCC;
                border-bottom: 1px solid #CCC;
                font: normal normal bold 13px Verdana, Geneva, Arial, Helvetica, sans-serif;
                padding: 5px;
                position: relative;
                text-align: left
            }

            .yui-skin-sam .yui-dt-bd tbody tr.yui-dt-even:hover, tbody tr.yui-dt-odd:hover, tbody tr.yui-dt-odd td.yui-dt-asc:hover, tbody tr.yui-dt-odd td.yui-dt-desc:hover, tbody tr.yui-dt-even td.yui-dt-asc:hover, tbody tr.yui-dt-even td.yui-dt-desc:hover {
                background-color:  #999;
                color: #FFF;
            }

            #buttonGroup{

                margin-top: 10px;
                margin-bottom: 10px
                margin-left: 10px;
            }


            .search_input    {
                width: 15em; /* Width for modern browsers */
                vertical-align: middle;
                padding: 0.1em;
                font-family:TrebuchetMS;
                font-size:9pt;
                font-weight:bold;
                color:000099;
            }

        </style>

        <style type="text/css">
            body {margin: 0; padding: 0;}
            .layout {width: 100%; border: none}
            .layout td {vertical-align: top; border: none}
            .columnL {width: 200px; border: none;}
            .columnR {width: 350px; border: none; padding-left: 10}

        </style>

        <!-- Advanced Search Box Style -->
        <style  type="text/css">

                                #hd, #bd { border: 1px solid #808080; }
                                #hd h1 { font-size: 120%; }
                                #bd { min-height: 350px; }
                                #bd p { border: 2px solid #426FD9; background-color: #F2F2F2; height: 200px; padding: .25em; }
                                #bd p.nav { background-color: #EDF5FF; border-color: #7D98B8; }
                                .bd { text-align: left; }

                                #toolBoxHolder { visibility: hidden; }
                                #toolBoxHolder fieldset { border: 1px solid #ccc; padding: .5em; margin: .5em 0; }
                                #toolBoxHolder p { padding-bottom: .25em }
                                #toolBoxHolder .yui-button { margin: .25em 0; }

                                #hd h1,
                                #ft p {
                                    min-height: 1.5em;
                                    height: 2em;
                                }

                                .first-row select {
                                    margin-left: 16px;
                                }

        </style>

        <!-- Reassign Box Style -->
        <style  type="text/css">

                                #reAssignToolBoxHolder { visibility: hidden; }
                                #reAssignToolBoxHolder fieldset { border: 1px solid #ccc; padding: .5em; margin: .5em 0; }
                                #reAssignToolBoxHolder p { padding-bottom: .25em }
                                #reAssignToolBoxHolder .yui-button { margin: .25em 0; }

                                #rhd h1,  #ft p{
                                    min-height: 1.5em;
                                    height: 2em;
                                }
                                .first-row select {
                                    margin-left: 16px;
                                }

        </style>

        <script type="text/javascript">
        </script>

        <title></title>
    </head>
    <body class="yui-skin-sam">
        <table class="layout" cellspacing="0">
            <tr>
                <td colspan="3">
                    <!-- background="../bg.jpg"> -->
                    <div id="header">
                        <h1>Open ESB Worklist Manager Console</h1>

                    </div>

                    <div id="welcome"><p><span>Welcome ! </span><%=remoteUser%></p></div>
                    <div id="pagetitle"><h1>       </h1></div>


                    <!--<%=remoteUser%></div>-->

                    <div id="myLogger"></div>
                    <!--header-->
                </td>
            </tr>
            <tr>
                <td class="columnL">
                    <!--left column here-->
                </td>
                <td class="content">
                    <!--content here-->
                    <div id="myContainer" class="yui-skin-sam" align="center" style="padding-top: 20px;"></div>

                    <div id="taskListActionContainer" align="center" class="yui-skin-sam"></div>
                    <form id="taskListActionForm" action="taskListAction.jsp">
                        <div id="buttonContainer" align="right" class="yui-skin-sam" style="padding-top: 20px;"></div>
                        <input id="taskId" type="hidden" name="taskId">
                        <input id="taskListAction" type="hidden" name="taskListAction">
                    </form>
                    <div align="center">
                        <iframe name="taskDetailsIframe" id="ifrm" class="container" align="center"
                                scrolling="auto" frameborder="0" height="500" width="850" style="padding-top: 10px;">An
                        iframe capable browser is required to view this web site.</iframe>
                    </div>

                </td>
                <td class="columnR">
                    <!--right column here-->

                    <div id="searchContainer" align="center" style="padding-top: 20px;">
                        <form id="searchActionForm">
                            <div>
                                <label for="searchText">Search Keywords:</label>
                                <input class="search_input" type="text" name="searchText" id="searchStr" />
                            </div>
                            <div id="buttonGroup">
                            </div>
                        </form>
                    </div>
                    <!-- For advanced search panel markup -->
                    <div id="panelBody">
                        <div id="toolBoxHolder">
                            <div class="hd">Advanced Search</div>
                            <div class="bd">
                                <form id="advancedSearchForm">
                                    <fieldset>
                                        <legend>Task Statuses</legend>
                                        <select id="taskStatus" multiple='multiple' style="width:200px;margin:5px 0 5px 0;">
                                            <option value="Assigned"  selected="selected">Assigned</option>
                                            <option value="Claimed"   selected="selected">Claimed</option>
                                            <option value="Escalated" selected="selected">Escalated</option>
                                            <option value="Completed">Completed</option>
                                            <option value="Failed">Failed</option>
                                        </select>
                                    </fieldset>
                                    <fieldset>
                                        <legend>Task Owners</legend>
                                        <div>
                                            <label for="users">Users (space-separated):</label>
                                            <input type="text" class="search_input" name="assignees" id="users" />
                                        </div>
                                        <div>
                                            <label for="Groups">Groups (space-separated):</label>
                                            <input type="text" class="search_input" name="groups" id="groups" />
                                        </div>
                                    </fieldset>
                                    <fieldset>
                                        <legend>Text Search</legend>
                                        <label for="searchText2">Search Keywords:</label>
                                        <input class="search_input" type="text" name="searchStr2" id="searchStr2" />
                                    </fieldset>

                                    <div>
                                        <button type="button" id="ASearch">Search</button>
                                        <button type="button" id="ACancel">Cancel</button>
                                    </div>
                                </form>
                            </div>
                        </div>
                    </div>
                    <!-- For reassign panel markup -->
                    <div id="reAssignPanelBody">
                        <div id="reAssignToolBoxHolder">
                            <div class="hd">Task Reassignment</div>
                            <div class="bd">
                                <form id="reAssignForm">
                                    <fieldset>
                                        <legend>Reassign to User</legend>
                                        <label for="reassign_user">User:</label>
                                        <input type="text" class="search_input" name="assignees" id="reassign_user" />
                                    </fieldset>
                                    <fieldset>
                                        <legend>Reassign to Group</legend>
                                        <label for="reassign_group">Group:</label>
                                        <input type="text" class="search_input" name="groups" id="reassign_group" />
                                    </fieldset>
                                    <div>
                                        <button type="button" id="ReAssignButton">ReAssign</button>
                                        <button type="button" id="ReAssignButtonCancel">Cancel</button>
                                    </div>
                                </form>
                            </div>
                        </div>
                    </div>

                </td>
            </tr>
            <tr>
                <td colspan="3" class="footer">
                    <!--footer-->
                </td>
            </tr>
        </table>

        <script type="text/javascript">
            var panelBodyEl = document.getElementById('panelBody');
            var searchStr2El = document.getElementById('searchStr2');
            var groupsEl = document.getElementById('groups');
            var usersEl = document.getElementById('users');
            var taskStatusEl = document.getElementById('taskStatus');
            var searchStrEl = document.getElementById("searchStr");
            var ifrmEl = document.getElementById('ifrm');
            var myContainerEl = document.getElementById('myContainer');
            var taskListActionContainerEl = document.getElementById('taskListActionContainer');
            var reAssignPanelBodyEl = document.getElementById('reAssignPanelBody');
            var reassign_userEl = document.getElementById('reassign_user');
            var reassign_groupEl = document.getElementById('reassign_group');

        </script>

        <script type="text/javascript">
            function calcHeight()
            {
                //find the height of the internal page
                var the_height=
                    ifrmEl.contentWindow.
                    document.body.scrollHeight;

                //change the height of the iframe
                ifrmEl.height=
                    the_height;


                //find the width of the tasklist table
                var the_width=
                    myContainerEl.scrollWidth;

                //change the width of the iframe
                ifrmEl.width=
                    the_width;


            }

        </script>
        <script type="text/javascript">



            // Initialize the temporary Panel to display while waiting for external content to load
            YAHOO.example.container.wait =
                new YAHOO.widget.Panel("wait",
            { width:"240px",
                fixedcenter:true,
                close:false,
                draggable:false,
                modal:true,
                visible:false,
                effect:{effect:YAHOO.widget.ContainerEffect.FADE, duration:0.5}
            }
        );

            YAHOO.example.container.wait.setHeader("Loading, please wait...");
            YAHOO.example.container.wait.setBody('<img src="../rel_interstitial_loading.gif" />');
            YAHOO.example.container.wait.render(document.body);

            function showWait() {
                // Show the Panel
                YAHOO.example.container.wait.show();

            }

            function hideWait() {
                // Show the Panel
                YAHOO.example.container.wait.hide();
            }

            function showIframe() {
                //var iframe = document.getElementById('ifrm');
                ifrmEl.style.display='inline';

            }

            function hideIframe() {
                //var iframe = document.getElementById('ifrm');
                ifrmEl.style.display='none';
            }

            function loadIframe(iframeName, url) {
                showIframe();

                if ( window.frames[iframeName] ) {
                    window.frames[iframeName].location = url;



                    return false;
                }
                else return true;
            }

            var checkOutButton = new YAHOO.widget.Button(
            { label: "Checkout",
                id: "checkoutButtonId",
                type: "button",
                container: "taskListActionForm",
                name: "taskListAction",
                value: "checkout"
            }
        );

            function onCheckOutButtonClick(e) {
                showWait();
                isWaiting = true;
                executeTaskListAction(selectedTaskId, "checkout");
                checkOutButton.set("disabled", true);
            }

            checkOutButton.addListener("click", onCheckOutButtonClick);
            checkOutButton.set("disabled", true);
            checkOutButton.setStyle("position", "relative");
            checkOutButton.setStyle("left", "10%");

            var completeButton = new YAHOO.widget.Button(
            { label: "Complete",
                id: "completeButtonId",
                type: "button",
                container: "taskListActionForm",
                name: "taskListAction",
                value: "complete"

            }
        );

            function onCompleteButtonClick(e) {
                showWait();
                hideIframe();
                isWaiting = true;

                executeTaskListAction(selectedTaskId, "complete");
                completeButton.set("disabled", true);
                reassignButton.set("disabled", true);
            }

            completeButton.addListener("click", onCompleteButtonClick);
            completeButton.set("disabled", true);
            completeButton.setStyle("position", "relative");
            completeButton.setStyle("left", "10%");

            var reassignButton = new YAHOO.widget.Button(
            { label: "Reassign",
                id: "reassignButtonId",
                type: "button",
                container: "taskListActionForm",
                name: "taskListAction",
                value: "reassign"

            }
        );

            function onReassignButtonClick(e) {
                showWait();
                reAssignUser = YAHOO.lang.trim(reassign_userEl.value);
                reAssignGroup = YAHOO.lang.trim(reassign_groupEl.value);
                hideIframe();
                reAssignToolBox.hide();
                isWaiting = true;
                executeTaskListAction(selectedTaskId, "reassign");
                reassignButton.set("disabled", true);
            }

            reassignButton.set("disabled", true);
            reassignButton.setStyle("position", "relative");
            reassignButton.setStyle("left", "10%");

            var searchButton = new YAHOO.widget.Button(
            { label: "Search",
                id: "SearchButtonId",
                type: "button",
                container: "searchActionForm",
                name: "searchAction",
                value: "Search"
            }
        );
            function onSearchButtonClick(e) {
                searchStatues = "";
                searchText = "";
                searchUsers = "";
                searchGroups = "";

                searchText = searchStrEl.value;
                searchType = 'SIMPLE';
                searchText = YAHOO.lang.trim(searchText);
                if (searchText == "") {
                    searchType = 'DEFAULT';
                    return;
                }

                showWait();
                hideIframe();
                selectedRow = null;
                selectedTaskId = null;
                selectedRecord = null;
                taskDetailsURL = null;

                isWaiting = true;
                inRefresh = false;

                pageStart = 0;
                refreshTaskList ();
            }

            function handleSubmit (e) {
                onSearchButtonClick(e);
                YAHOO.util.Event.preventDefault(e);
            }

            searchButton.addListener("click", onSearchButtonClick);
            YAHOO.util.Event.addListener("searchActionForm", "submit", handleSubmit);

            var resetButton = new YAHOO.widget.Button(
            { label: "Reset",
                id: "ResetButtonId",
                type: "button",
                container: "searchActionForm",
                name: "resetAction",
                value: "Reset"
            });

            function onResetButtonClick(e) {

                showWait();
                searchStatues = "";
                searchText = "";
                searchUsers = "";
                searchGroups = "";
                searchType = 'DEFAULT';
                // var searchInput = searchStrEl;

                searchStrEl.value = "";

                hideIframe();
                //showWait();

                selectedRow = null;
                selectedTaskId = null;
                selectedRecord = null;
                taskDetailsURL = null;

                isWaiting = true;
                inRefresh = false;

                pageStart = 0;
                refreshTaskList ();
            }

            resetButton.addListener("click", onResetButtonClick);

            var advanceButton = new YAHOO.widget.Button(
            { label: "Advance",
                id: "AdvanceButtonId",
                type: "button",
                container: "searchActionForm",
                name: "advanceSearchAction",
                value: "AdvanceSearch"
            });

            function onAdvancedSearchCancel(e) {
                toolBox.hide();
            }

            function onReassignCancel(e)	{
                reAssignToolBox.hide();
            }

            function onAdvancedSearch(e) {

                showWait();
                searchStatues = "";
                searchText = "";
                searchUsers = "";
                searchGroups = "";
                searchType = 'COMPLEX';
                var ob = taskStatusEl;

                for (var i = 0; i < ob.options.length; i++) {
                    if (ob.options[i].selected) {
                        searchStatues = searchStatues + " " + ob.options[i].value;
                    }
                }

                searchStatues = YAHOO.lang.trim(searchStatues);

                searchUsers = usersEl.value;
                searchUsers = YAHOO.lang.trim(searchUsers);

                searchGroups = groupsEl.value;
                searchGroups = YAHOO.lang.trim(searchGroups);

                searchText = searchStr2El.value;
                searchText = YAHOO.lang.trim(searchText);


                //alert ("searchStatues:" + searchStatues);
                //alert ("searchUsers:" + searchUsers);
                //alert ("searchGroups:" + searchGroups);
                //alert ("searchKeywords:" + searchText);
                hideIframe();
                toolBox.hide();
                //showWait();

                selectedRow = null;
                selectedTaskId = null;
                selectedRecord = null;
                taskDetailsURL = null;

                isWaiting = true;
                inRefresh = false;

                pageStart = 0;
                refreshTaskList ();
            }

            function showAdvanceSearch() {
                toolBox = new YAHOO.widget.Panel('toolBoxHolder', {
                    draggable: true,
                    constraintoviewport: true,
                    close: true,
                    visible: false,
                    //xy: [1400, 300],
                    context:["buttonGroup","tl","bl"],
                    width: '300px'
                }
            );
                toolBox.render(panelBodyEl);
                var advancedSearchButton = new YAHOO.widget.Button('ASearch');
                advancedSearchButton.addListener("click", onAdvancedSearch);
                var advancedCancelButton = new YAHOO.widget.Button('ACancel');
                advancedCancelButton.addListener("click", onAdvancedSearchCancel);
                toolBox.show();
            }

            advanceButton.addListener("click", showAdvanceSearch);

            function showReassignmentBox() {
                reAssignToolBox = new YAHOO.widget.Panel('reAssignToolBoxHolder', {
                    draggable: true,
                    constraintoviewport: true,
                    close: true,
                    visible: false,
                    xy: [1400, 300],
                    context:["buttonContainer","tl","bl"],
                    width: '275px'
                }
            );
                reAssignToolBox.render(reAssignPanelBodyEl);
                var reAssignDo = new YAHOO.widget.Button('ReAssignButton');
                reAssignDo.addListener("click", onReassignButtonClick);
                var reAssignCancelButton = new YAHOO.widget.Button('ReAssignButtonCancel');
                reAssignCancelButton.addListener("click", onReassignCancel);
                reAssignToolBox.show();
            }

            reassignButton.addListener("click", showReassignmentBox);


        </script><script type="text/javascript">

            //var myLogReader = new YAHOO.widget.LogReader("myLogger");
        </script><script type="text/javascript">


            var selectedrow;

            var selectedTaskId;

            var isWaiting = false;

            var myDataSource = new YAHOO.util.DataSource(taskListURL);

            var myColumnHeaders = [
                {key:"id", label:"Task Id", type:"number", sortable:true},
                {key:"title", label:"Title", sortable:true},
                {key:"createDate", label:"Submitted On", type:"date", sortable:true},
                {key:"priority",  label:"Priority",  sortable:true},
                {key:"status", label:"Status", sortable:true},
                {key:"assignedTo", label:"Assigned To", sortable:true},
                {key:"owner", label:"Claimed By", sortable:true},
                {key:"deadline", label:"Deadline", type:"date", sortable:true}
            ];

            var myColumnSet = new YAHOO.widget.ColumnSet(myColumnHeaders);

            // Set the responseType as XML
            myDataSource.responseType = YAHOO.util.DataSource.TYPE_XML;

            // Define the data schema
            myDataSource.responseSchema = {
                resultNode: "task", // Node name of each result item
                fields: ["id","title","createDate", "priority", "status", "assignedTo","owner","deadline"],
                metaFields: {
                    totalRecords: "total",
                    returnedRecords: "returned" }                // Field names
            };

            var initReq = "start=0&pageSize=" + pageSize + "&sort=" + savedSort + "&dir=" + savedDir;

            var paginatorConfig = { rowsPerPage    : pageSize};

            var myPaginator = new YAHOO.widget.Paginator(paginatorConfig);

            var myRequestBuilder = function(oState, oSelf) {
                // Get states or use defaults
                oState = oState || {pagination:null, sortedBy:null};
                var sort = (oState.sortedBy) ? oState.sortedBy.key : savedSort;
                var dir = (oState.sortedBy && oState.sortedBy.dir === YAHOO.widget.DataTable.CLASS_DESC) ? "DESC" : savedDir;

                var startIndex = (oState.pagination) ? oState.pagination.recordOffset : 0;
                var results = (oState.pagination) ? oState.pagination.rowsPerPage : pageSize;

                pageStart = startIndex;
                pageSize = results;
                // Build custom request
                savedSort = sort;
                savedDir = dir;

                return  "start=" + pageStart + "&pageSize=" + results + "&sort=" + sort + "&dir=" + dir;
                //return  "start=0&pageSize=" + pageSize;
            };

            var myConfigs = {
                initialRequest : initReq,
                dynamicData : true,
                sortedBy : {key : savedSort , dir :  YAHOO.widget.DataTable.CLASS_ASC},
                paginator : myPaginator,
                paginationEventHandler : YAHOO.widget.DataTable.handleDataSourcePagination,
                generateRequest : myRequestBuilder,
                rowSingleSelect : true
                // scrollable : true,
                // height : "200px"
            };

            var myDataTable = new YAHOO.widget.DataTable("myContainer",
            myColumnSet,
            myDataSource,
            myConfigs);


            myDataTable.doBeforeLoadData = function(sRequest, oResponse, oPayload) {
                oPayload.totalRecords = oResponse.meta.totalRecords;
                oldPageRecords = currentPageRecords;
                currentPageRecords = parseInt(oResponse.meta.returnedRecords);
                totalRecords = parseInt (oResponse.meta.totalRecords);
                return oPayload;
            }

            myDataSource.doBeforeCallback = function(sRequest, oFullResponse, oParsedResponse) {
                oldPageRecords = currentPageRecords;
                currentPageRecords = parseInt(oParsedResponse.meta.returnedRecords);
                totalRecords = parseInt (oParsedResponse.meta.totalRecords);
                return oParsedResponse;
            }

            // Clicking on a TD element or Row will trigger row selection.
            // myDataTable.subscribe("cellClickEvent", rowSelectEvent);
            myDataTable.subscribe("rowClickEvent",  rowSelectEvent);

            // Select the first row immediately
            myDataTable.select(myDataTable.getRow(0));


            function rowSelectEvent(oArgs) {

                myDataTable.unselectAllRows ();

                myDataTable.selectRow(oArgs.target);

                selectedRow = myDataTable.getSelectedRows();

                var theSelectedRow = myDataTable.getRecord(selectedRow[0]);

                var taskId = theSelectedRow.getData("id");

                selectedTaskId = taskId;

                var claimedBy = theSelectedRow.getData("owner");

                // checkOutButton.setStyle("visibility", "hidden");
                // completeButton.setStyle("visibility", "hidden");
                checkOutButton.set("disabled", true);
                completeButton.set("disabled", true);
                reassignButton.set("disabled", false);

                if(taskId != null && (claimedBy == null || claimedBy == '')) {
                    //checkOutButton.setStyle("visibility", "visible");
                    checkOutButton.set("disabled", false);
                }

                if(taskId != null && claimedBy != null && claimedBy != '') {
                    //completeButton.setStyle("visibility", "visible");
                    completeButton.set("disabled", false);
                }

                showTaskDetails(taskId, claimedBy, true);
            }

        </script>
    </body>
</html>

