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
 * @(#)SimulatorBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.simulator;

import java.io.ByteArrayOutputStream;
import java.util.logging.Logger;
/**
 *
 * @author ylee
 */
public class SimulatorBean {
    
    private String destinationUri = "";
    private String soapAction = "";
    private String soapPayload = "";
    private String soapResult = "";
    
    private Simulator simulator;
    
    private Logger logger = Logger.getLogger(SimulatorBean.class.getName());
    
    /** Creates a new instance of Simulator */
    public SimulatorBean() {
        simulator = new Simulator();
    }
    
    public void setDestinationUri(String value) {
        destinationUri = value;
    }
    
    public String getDestinationUri() {
        return destinationUri;
    }
    
    public void setSoapAction(String value) {
        soapAction = value;
    }
    
    public String getSoapAction() {
        return soapAction;
    }
    
    public void setSoapPayload(String value) {
        soapPayload = value;
    }
    
    public String getSoapPayload() {
        return soapPayload;
    }
    
    public String getSoapResult() {
        return soapResult;
    }
    
    public void setSoapResult(String result) {
        soapResult = result;
    }
    
    public void submit() {
        logger.info("submit...");
        simulator.setDestinationURI(destinationUri);
        simulator.setSoapAction(soapAction);
        simulator.setSoapRequest(soapPayload);
        
        try {
           ByteArrayOutputStream s = simulator.execute();
           logger.info("soap reply: "+s);
           soapResult = s.toString();
        } catch(Exception e) {
            e.printStackTrace();
        }
    }
    
    public void clear() {
        logger.info("clear...");
        destinationUri = "";
        soapAction = "";
        soapPayload = "";
        soapResult = "";
    }
}
