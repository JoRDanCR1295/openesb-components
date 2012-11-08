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
 * @(#)CMMSG.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/


package com.sun.jbi.hl7bc.extservice.ack.hl7v22;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.sun.jbi.hl7bc.extservice.ack.hl7v22.CMMSG;
import com.sun.jbi.hl7bc.extservice.ack.hl7v22.CMMSG1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v22.CMMSG2CONTENT;


/**
 * <p>Java class for CM_MSG complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CM_MSG">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}CM_MSG.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CM_MSG.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CM_MSG", namespace = "urn:hl7-org:v2xml", propOrder = {
    "cmmsg1",
    "cmmsg2"
})
public class CMMSG {

    @XmlElement(name = "CM_MSG.1", namespace = "urn:hl7-org:v2xml")
    protected CMMSG1CONTENT cmmsg1;
    @XmlElement(name = "CM_MSG.2", namespace = "urn:hl7-org:v2xml")
    protected CMMSG2CONTENT cmmsg2;

    /**
     * Gets the value of the cmmsg1 property.
     * 
     * @return
     *     possible object is
     *     {@link CMMSG1CONTENT }
     *     
     */
    public CMMSG1CONTENT getCMMSG1() {
        return cmmsg1;
    }

    /**
     * Sets the value of the cmmsg1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CMMSG1CONTENT }
     *     
     */
    public void setCMMSG1(CMMSG1CONTENT value) {
        this.cmmsg1 = value;
    }

    /**
     * Gets the value of the cmmsg2 property.
     * 
     * @return
     *     possible object is
     *     {@link CMMSG2CONTENT }
     *     
     */
    public CMMSG2CONTENT getCMMSG2() {
        return cmmsg2;
    }

    /**
     * Sets the value of the cmmsg2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CMMSG2CONTENT }
     *     
     */
    public void setCMMSG2(CMMSG2CONTENT value) {
        this.cmmsg2 = value;
    }

}
