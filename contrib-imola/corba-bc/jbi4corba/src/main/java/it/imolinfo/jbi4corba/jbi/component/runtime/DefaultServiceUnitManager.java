 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
/*
 * DefaultServiceUnitManager.java
 *
 */

package it.imolinfo.jbi4corba.jbi.component.runtime;

import javax.jbi.component.ServiceUnitManager;

/**
 * Default Service Unit Manager implementation. Component's supporting the
 * deployment should extend this class to support the service unit deployment.
 *
 * @see javax.jbi.ServiceUnitManager
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public abstract class DefaultServiceUnitManager implements ServiceUnitManager {
	
    /**
     * Component runtime as context
     */
    @SuppressWarnings("unused")
	private ComponentRuntime mContext;
    

    /**
     * constructor that takes the compoent runtime
     */
    public DefaultServiceUnitManager(ComponentRuntime ctx) {
        this.mContext = ctx;
    }    

    ///////////////////////////////////////////////////////////////////////////
    // Helper methods
    ///////////////////////////////////////////////////////////////////////////    
    /**
     * Helper method to create result message in management message xml.
     * @param message message string to return
     * @param isSuccess true to format a sucess result, false to format a failed result.
     * @return XML string.
     */
    protected String createComponentTaskResultXML(String message, boolean isSuccess) {
        
        String taskResult = isSuccess ? "SUCCESS" : "FAILED";
        String msgType = isSuccess ? "INFO" : "ERROR";
        String componentName = RuntimeHelper.getComponentName();
        
        String xmlResult =
            "<component-task-result xmlns=\"http://java.sun.com/xml/ns/jbi/management-message\" >" +
            "  <component-name>" + componentName + "</component-name>" +
            "  <component-task-result-details >" +
            "      <task-result-details>" +
            "          <task-id>deployment</task-id>" +
            "          <task-result>" + taskResult + "</task-result>" +
            "          <message-type>" + msgType + "</message-type>" +
            "          <task-status-msg>" +
            "             <msg-loc-info>" +
            "                <loc-token>MSG_ID_000</loc-token>" +
            "                <loc-message>" + message + "</loc-message>" +
            "              </msg-loc-info>" +
            "          </task-status-msg>" +
            "      </task-result-details>" +
            "  </component-task-result-details>" +
            "</component-task-result>";
        
        return xmlResult;
    }
    
}
