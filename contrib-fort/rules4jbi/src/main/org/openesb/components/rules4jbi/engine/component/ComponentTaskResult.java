/*
 * @(#)ComponentTaskResult.java        $Revision: 1.2 $ $Date: 2008/07/14 16:30:25 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.engine.component;

import nu.xom.Element;

import net.jcip.annotations.Immutable;

import org.openesb.components.rules4jbi.shared.util.XOMUtils;

/**
 * This class represents a component status result string, as defined in JBI spec, section [6.9].
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/14 16:30:25 $
 * 
 * @since 0.1
 */
@Immutable
public class ComponentTaskResult {
    
    private static final String MANAGEMENT_MESSAGE_NAMESPACE_URI =
            "http://java.sun.com/xml/ns/jbi/management-message";
    
    private static final String FAILED_TASK_RESULT_STRING = "FAILED";
    
    private static final String SUCCESSFUL_TASK_RESULT_STRING = "SUCCESS";

    private final String componentName;
    
    private final String taskID;
    
    private final String taskResult;

    private final Element root;
    
    public ComponentTaskResult(String componentName, String taskID, boolean failed) {
        this.componentName = componentName;
        this.taskID = taskID;
        taskResult = failed ? FAILED_TASK_RESULT_STRING : SUCCESSFUL_TASK_RESULT_STRING;
        
        root = createTaskResult(componentName, taskID, taskResult);
    }
    
    static Element createTaskResult(String componentName, String taskID, String taskResult) {
        Element componentNameElement = new Element("component-name", MANAGEMENT_MESSAGE_NAMESPACE_URI);
        componentNameElement.appendChild(componentName);
        
        Element taskIDElement = new Element("task-id", MANAGEMENT_MESSAGE_NAMESPACE_URI);
        taskIDElement.appendChild(taskID);
        
        Element taskResultElement = new Element("task-result", MANAGEMENT_MESSAGE_NAMESPACE_URI);
        taskResultElement.appendChild(taskResult);
        
        Element taskResultDetails = new Element("task-result-details", MANAGEMENT_MESSAGE_NAMESPACE_URI);
        taskResultDetails.appendChild(taskIDElement);
        taskResultDetails.appendChild(taskResultElement);
        
        Element componentTaskResultDetails =
                new Element("component-task-result-details", MANAGEMENT_MESSAGE_NAMESPACE_URI);
        componentTaskResultDetails.appendChild(taskResultDetails);
        
        Element componentTaskResult = new Element("component-task-result", MANAGEMENT_MESSAGE_NAMESPACE_URI);
        componentTaskResult.appendChild(componentNameElement);
        componentTaskResult.appendChild(componentTaskResultDetails);
        
        return componentTaskResult;
    }
    
    public String getComponentName() {
        return componentName;
    }

    public String getTaskID() {
        return taskID;
    }

    public String getTaskResult() {
        return taskResult;
    }

    public String toXML() {
        return root.toXML();
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Component Task Result\n");
        final String indent = "    ";
        
        sb.append(indent);
        sb.append("component-name: ");
        sb.append(componentName);
        sb.append("\n");
        
        sb.append(indent);
        sb.append("task-id: ");
        sb.append(taskID);
        sb.append("\n");
        
        sb.append(indent);
        sb.append("task-result: ");
        sb.append(taskResult);
        
        return sb.toString();
    }
    
    public static void main(String[] args) {
        ComponentTaskResult componentTaskResult = new ComponentTaskResult("test-rule-engine", "deploy", true);
        
        System.out.println(componentTaskResult);
        System.out.println();
        System.out.println(componentTaskResult.toXML());
        System.out.println();
        XOMUtils.prettyPrint(componentTaskResult.root);
    }
}
