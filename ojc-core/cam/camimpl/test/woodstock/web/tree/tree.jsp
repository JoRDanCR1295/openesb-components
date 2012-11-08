<jsp:root version="1.2" 
          xmlns:f="http://java.sun.com/jsf/core" 
          xmlns:h="http://java.sun.com/jsf/html" 
          xmlns:jsp="http://java.sun.com/JSP/Page" 
          xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
    
    <jsp:directive.page contentType="text/html" /> 
    
    <f:view>
        
        <webuijsf:page id="page1">
            <webuijsf:html id="html1" >
                <webuijsf:head id="head1" title="Tree">
                </webuijsf:head>
                <webuijsf:form id="treeForm">
                    <webuijsf:tree id="tree" url="#">
                        
                        <f:facet name="content">
                            <h:panelGroup>
                                
                                <webuijsf:button id="refresh" text="Refresh" 
                                                 toolTip="refresh" 
                                                 mini="true" 
                                />
                                
                            </h:panelGroup>
                        </f:facet>
                        
                        <f:facet name="content">
                            <webuijsf:dropDown id="airport" submitForm="true" items="#{BackingFileChoice.airports}" />
                        </f:facet>
                        
                        <webuijsf:treeNode id="node0" expanded="true" text="Paducah" url="#">
                            <f:facet name="image">
                                <webuijsf:image id="image" icon="TREE_FOLDER_ALARM_MINOR" />
                            </f:facet>
                            <webuijsf:treeNode id="node1" expanded="true" text="Building 1" url="#">
                                <f:facet name="image">
                                    <webuijsf:image id="image" icon="TREE_FOLDER_ALARM_MINOR" />
                                </f:facet>
                                <webuijsf:treeNode id="node1_1" text="Kenga">
                                    <f:facet name="image">
                                        <webuijsf:image id="image" icon="TREE_SERVER" />
                                    </f:facet>
                                </webuijsf:treeNode>
                                <webuijsf:treeNode id="node1_2" text="Crocker" url="#">
                                    <f:facet name="image">
                                        <webuijsf:image id="image" icon="TREE_SERVER_MINOR" />
                                    </f:facet>
                                </webuijsf:treeNode>
                            </webuijsf:treeNode>
                            
                            <webuijsf:treeNode id="node5" expanded="true" text="Building 4" url="#">
                                <f:facet name="image">
                                    <webuijsf:image id="image" icon="TREE_FOLDER_ALARM_MAJOR" />
                                </f:facet>
                                <webuijsf:treeNode id="node5_1" text="Neptune" url="#">
                                    <f:facet name="image">
                                        <webuijsf:image id="image" icon="TREE_STORAGE_MAJOR" />
                                    </f:facet>
                                </webuijsf:treeNode>
                                <webuijsf:treeNode id="node5_2" text="Zeus" url="#">
                                    <f:facet name="image">
                                        <webuijsf:image id="image" icon="TREE_STORAGE" />
                                    </f:facet>
                                </webuijsf:treeNode>
                            </webuijsf:treeNode>
                        </webuijsf:treeNode>    
                        
                        
                    </webuijsf:tree>   
                </webuijsf:form>     
            </webuijsf:html>
        </webuijsf:page>
        
    </f:view>
</jsp:root>
