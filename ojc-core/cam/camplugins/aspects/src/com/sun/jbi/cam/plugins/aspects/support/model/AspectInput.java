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
 * @(#)AspectInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model;

import java.io.Serializable;

import javax.xml.namespace.QName;

/**
 * @author graj
 *
 */
public class AspectInput implements Serializable {
	private static final long serialVersionUID = 1L;
	
    QName partnerLinkQName;
    String roleName;
    QName portTypeQName;
    String operationName;
    QName messageTypeQName;
    String inputTransformation;
    boolean transformJBIMessage;
    

    /**
     * 
     */
    public AspectInput() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @param partnerLinkQName
     * @param roleName
     * @param portTypeQName
     * @param operationName
     * @param messageTypeQName
     * @param inputTransformation
     * @param transformJBIMessage
     */
    public AspectInput(QName partnerLinkQName, String roleName, QName portTypeQName, String operationName, QName messageTypeQName, String inputTransformation, boolean transformJBIMessage) {
        super();
        this.partnerLinkQName = partnerLinkQName;
        this.roleName = roleName;
        this.portTypeQName = portTypeQName;
        this.operationName = operationName;
        this.messageTypeQName = messageTypeQName;
        this.inputTransformation = inputTransformation;
        this.transformJBIMessage = transformJBIMessage;
    }

    /**
     * @return the inputTransformation
     */
    public String getInputTransformation() {
        return inputTransformation;
    }

    /**
     * @param inputTransformation the inputTransformation to set
     */
    public void setInputTransformation(String inputTransformation) {
        this.inputTransformation = inputTransformation;
    }

    /**
     * @return the messageTypeQName
     */
    public QName getMessageTypeQName() {
        return messageTypeQName;
    }

    /**
     * @param messageTypeQName the messageTypeQName to set
     */
    public void setMessageTypeQName(QName messageTypeQName) {
        this.messageTypeQName = messageTypeQName;
    }

    /**
     * @return the operationName
     */
    public String getOperationName() {
        return operationName;
    }

    /**
     * @param operationName the operationName to set
     */
    public void setOperationName(String operationName) {
        this.operationName = operationName;
    }

    /**
     * @return the roleName
     */
    public String getRoleName() {
        return roleName;
    }

    /**
     * @param roleName the roleName to set
     */
    public void setRoleName(String roleName) {
        this.roleName = roleName;
    }

    /**
     * @return the portTypeQName
     */
    public QName getPortTypeQName() {
        return portTypeQName;
    }

    /**
     * @param portTypeQName the portTypeQName to set
     */
    public void setPortTypeQName(QName portTypeQName) {
        this.portTypeQName = portTypeQName;
    }

    /**
     * @return the partnerLinkQName
     */
    public QName getPartnerLinkQName() {
        return partnerLinkQName;
    }

    /**
     * @param partnerLinkQName the partnerLinkQName to set
     */
    public void setPartnerLinkQName(QName partnerLinkQName) {
        this.partnerLinkQName = partnerLinkQName;
    }

    /**
     * @return the transformJBIMessage
     */
    public boolean isTransformJBIMessage() {
        return transformJBIMessage;
    }

    /**
     * @param transformJBIMessage the transformJBIMessage to set
     */
    public void setTransformJBIMessage(boolean transformJBIMessage) {
        this.transformJBIMessage = transformJBIMessage;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
