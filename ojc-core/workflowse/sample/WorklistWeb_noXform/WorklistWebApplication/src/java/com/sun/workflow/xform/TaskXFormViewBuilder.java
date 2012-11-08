/*
 * TaskXFormViewBuilder.java
 * 
 * Created on Apr 25, 2007, 6:45:24 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.workflow.xform;

import com.sun.workflow.xml.Util;
import java.io.FileOutputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author radval
 */
public class TaskXFormViewBuilder {

    private String mTaskId;
    
    private String mClaimedBy;
    
    private String mUserId;
    
    public static final String  XHTML_NS ="http://www.w3.org/1999/xhtml";
    
    public static final String  XFORM_NS ="http://www.w3.org/2002/xforms";
    
    private  boolean debug = false;
    
    public TaskXFormViewBuilder(String taskId, String claimedBy, String userId) {
        this.mTaskId = taskId;
        this.mClaimedBy = claimedBy;
        this.mUserId = userId;
    }
    
    public Node build() {
        Node viewNode = null;
        
        try {
            viewNode = getTaskXForm();
            
            if(mClaimedBy == null || mClaimedBy.trim().equals("")) {
                viewNode = removeOutputXFormView(viewNode);
                if(debug) {
                    String xml = Util.toXml(viewNode, "UTF-8", true);
                    System.out.println("after removeOutputXFormView: " + xml);
                }
            }

            Node taskInput = getTaskInput();
            if(taskInput != null) {
                insertXformInputModel(viewNode, taskInput);
                if(debug) {
                    String xml = Util.toXml(viewNode, "UTF-8", true);
                    System.out.println("after insertXformModel for input: " + xml);
                }
            }

            Node taskOutput = getTaskOutput();
            if(taskOutput != null) {
                insertXformOutputModel(viewNode, taskOutput);
                if(debug) {
                    String xml = Util.toXml(viewNode, "UTF-8", true);
                    System.out.println("after insertXformModel for output: " + xml);
                }
            }
        } catch(Exception ex) {
            ex.printStackTrace();;
        }
        return viewNode;
    }
    
    private Node getTaskXForm() throws Exception {
        Node taskXFormNode = null;
         if(mTaskId != null) {
          try {
                com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
                com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();
                java.lang.Object result = port.getTaskXForm(Long.parseLong(mTaskId));
                
                if (result != null && result instanceof Node) {
                    taskXFormNode = (Node) result;
                    if(debug) {
                        String xml = Util.toXml(taskXFormNode, "UTF-8", true);
                        System.out.println("getTaskXForm: " + xml);
                        xml = new String(xml.getBytes("ISO-8859-1"),"UTF-8");
                    }
                }
                
            } catch (Exception ex) {
                ex.printStackTrace();
            }
         }
        
        if(taskXFormNode != null) {
         //getTaskXForm api returns a top level wrapper node
          //which we need to remove
            Document document  = taskXFormNode.getOwnerDocument();
            NodeList htmlList = document.getElementsByTagNameNS(XHTML_NS, "html");
            if(htmlList != null && htmlList.getLength() == 1) {
                taskXFormNode = htmlList.item(0);
                DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
                Document modelDocument =  builder.newDocument();
                taskXFormNode = modelDocument.importNode(taskXFormNode, true);
                modelDocument.appendChild(taskXFormNode);
            }
        }
        return taskXFormNode;
    }
    
    private Node getTaskInput() {
        Node taskInputNode = null;
             if(mTaskId != null) {
                    try {
                        com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
                        com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();
                        java.lang.Object result = port.getTaskInput(Long.parseLong(mTaskId), mUserId);
                        
                        if (result != null && result instanceof Node) {
                            taskInputNode = (Node) result;
                            if(debug) {
                                String xml = Util.toXml(taskInputNode, "UTF-8", true);
                                System.out.println("getTaskInput: " + xml);
                            }
                        }
                    } catch (Exception ex) {
                        ex.printStackTrace();
                    }
                }
        
            return taskInputNode;
    }
    
    private Node getTaskOutput() {
        Node taskOutputNode = null;
        if(mClaimedBy != null && !mClaimedBy.trim().equals("")) {
            if(mTaskId != null) {
                try {
                    com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
                    com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();
                    java.lang.Object result = port.getTaskOutput(Long.parseLong(mTaskId), mUserId);
                
                    if (result != null && result instanceof Node) {
                        taskOutputNode = (Node) result;
                        if(debug) {
                            String xml = Util.toXml(taskOutputNode, "UTF-8", true);
                            System.out.println("getTaskOutput: " + xml);
                        }
                    }
                
                
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        }
            return taskOutputNode;
    }
    
    private void insertXformInputModel(Node xhtmlXformViewNode, Node taskInputInstanceNode) throws Exception {
        DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        Document modelDocument =  builder.newDocument();
        
        Element modelElement = modelDocument.createElementNS(XFORM_NS, "model");
        modelElement.setAttribute("id", "inputModel");
        modelDocument.appendChild(modelElement);
        
        Element instanceElement = modelDocument.createElementNS(XFORM_NS, "instance");
        instanceElement.setAttribute("id", "input");
        
        Node importedTaskInstanceNode = modelDocument.importNode(taskInputInstanceNode, true);
        instanceElement.appendChild(importedTaskInstanceNode);
        modelElement.appendChild(instanceElement);
        modelDocument.normalizeDocument();
        
        insertXformModel(xhtmlXformViewNode, modelElement);
    }
    
    private void insertXformOutputModel(Node xhtmlXformViewNode, Node taskOutputInstanceNode) throws Exception {
        DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        Document modelDocument =  builder.newDocument();
        
        Element modelElement = modelDocument.createElementNS(XFORM_NS, "model");
        modelElement.setAttribute("id", "outputModel");
        modelDocument.appendChild(modelElement);
        
        Element instanceElement = modelDocument.createElementNS(XFORM_NS, "instance");
        instanceElement.setAttribute("id", "output");
        
        Node importedTaskInstanceNode = modelDocument.importNode(taskOutputInstanceNode, true);
        instanceElement.appendChild(importedTaskInstanceNode);
        modelElement.appendChild(instanceElement);
        
        //<xforms:submission id="default-submission" method="post" action="../worklist/save.view?taskId=<%=taskId%>" separator="&amp;"
        //replace="all"/>
        Element submissionElement = modelDocument.createElementNS(XFORM_NS, "submission");
        submissionElement.setAttribute("id", "default-submission");
        submissionElement.setAttribute("method", "post");
        submissionElement.setAttribute("separator", "&");
        submissionElement.setAttribute("action", "../worklist/save.view?taskId="+mTaskId + "&userId=" + mUserId + "&claimedBy="+ mClaimedBy);
        submissionElement.setAttribute("replace", "all");
        modelElement.appendChild(submissionElement);
        
        
        modelDocument.normalizeDocument();
        
        insertXformModel(xhtmlXformViewNode, modelElement);
    }
    
    private void insertXformModel(Node xhtmlXformViewNode, Node xformModelNode) {
        Node headNode = null;
        Document document = xhtmlXformViewNode.getOwnerDocument();
        NodeList headNodes = document.getElementsByTagNameNS(XHTML_NS, "head");
        if(headNodes != null && headNodes.getLength() != 0) {
            headNode = headNodes.item(0);
        } else {
            headNode = document.createElementNS(XHTML_NS, "head");
        }
        Node importedNode = document.importNode(xformModelNode, true);
        headNode.appendChild(importedNode);
        document.normalizeDocument();
    }
    
    private Node removeOutputXFormView(Node xformViewNode) {
        //if not claimed by then we need to remove outputXForm.
        
       Document document = xformViewNode.getOwnerDocument();
        NodeList nList = document.getElementsByTagNameNS(XHTML_NS, "outputXForm");
        if(nList.getLength() == 1) {
            Node outputXFormNode = nList.item(0);
            Node parent = outputXFormNode.getParentNode();

            if(parent != null) {
                parent.removeChild(outputXFormNode);
            }
        }

         return xformViewNode;
    }
}
