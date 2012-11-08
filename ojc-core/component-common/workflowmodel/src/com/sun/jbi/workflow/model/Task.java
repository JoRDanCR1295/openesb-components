/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)Task.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model;

import java.util.List;
import java.util.Map;

import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.apache.commons.jxpath.JXPathContext;

import com.sun.jbi.workflow.model.xmlbeans.TActionType;



public interface Task extends ModelElement, PortTypeModelElement, TaskElement {
    
    String getName();
    
    Assignment getTaskAssignment();
    
    List<Timeout> getTaskTimeouts();
    
    List<Escalation> getTaskEscalations();
    
    List<Notification> getTaskNotifications();
    
    List<Action> getTaskActions();
    
    DeadlineOrDuration findDeadlineOrDuration(String xpath);
    
    String getTargetNamespace();
    
    
    /**
     * Returns a list of WSDLDefinition this Tasks imports
     * @return A list of WSDLDefinition, never null
     * @throws ModelException 
     */
    List<Definition> getImportWSDLs() throws ModelException;
    
    Map<String, String> getTotalNamespaces ();
     
     Init  getInit ();
     
     List<Keyword> getKeywords ();
     
     /**
      * get Notification matching QName
      * @parm notificationQName should have namespace and localName
      **/
     Notification getNotification(String notificationQName);
     
     Action getAction (TActionType.Enum type);
     
     QName getQName ();
     
     int getPriority (JXPathContext context) throws ModelException;
     
     String getTitle (JXPathContext context) throws ModelException;
     
     
}
