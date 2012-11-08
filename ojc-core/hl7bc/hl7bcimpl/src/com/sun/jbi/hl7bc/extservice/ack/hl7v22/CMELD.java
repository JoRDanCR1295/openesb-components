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
 * @(#)CMELD.java 
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
import com.sun.jbi.hl7bc.extservice.ack.hl7v22.CMELD;
import com.sun.jbi.hl7bc.extservice.ack.hl7v22.CMELD1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v22.CMELD2CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v22.CMELD3CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v22.CMELD4CONTENT;


/**
 * <p>Java class for CM_ELD complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CM_ELD">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}CM_ELD.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CM_ELD.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CM_ELD.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CM_ELD.4" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CM_ELD", namespace = "urn:hl7-org:v2xml", propOrder = {
    "cmeld1",
    "cmeld2",
    "cmeld3",
    "cmeld4"
})
public class CMELD {

    @XmlElement(name = "CM_ELD.1", namespace = "urn:hl7-org:v2xml")
    protected CMELD1CONTENT cmeld1;
    @XmlElement(name = "CM_ELD.2", namespace = "urn:hl7-org:v2xml")
    protected CMELD2CONTENT cmeld2;
    @XmlElement(name = "CM_ELD.3", namespace = "urn:hl7-org:v2xml")
    protected CMELD3CONTENT cmeld3;
    @XmlElement(name = "CM_ELD.4", namespace = "urn:hl7-org:v2xml")
    protected CMELD4CONTENT cmeld4;

    /**
     * Gets the value of the cmeld1 property.
     * 
     * @return
     *     possible object is
     *     {@link CMELD1CONTENT }
     *     
     */
    public CMELD1CONTENT getCMELD1() {
        return cmeld1;
    }

    /**
     * Sets the value of the cmeld1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CMELD1CONTENT }
     *     
     */
    public void setCMELD1(CMELD1CONTENT value) {
        this.cmeld1 = value;
    }

    /**
     * Gets the value of the cmeld2 property.
     * 
     * @return
     *     possible object is
     *     {@link CMELD2CONTENT }
     *     
     */
    public CMELD2CONTENT getCMELD2() {
        return cmeld2;
    }

    /**
     * Sets the value of the cmeld2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CMELD2CONTENT }
     *     
     */
    public void setCMELD2(CMELD2CONTENT value) {
        this.cmeld2 = value;
    }

    /**
     * Gets the value of the cmeld3 property.
     * 
     * @return
     *     possible object is
     *     {@link CMELD3CONTENT }
     *     
     */
    public CMELD3CONTENT getCMELD3() {
        return cmeld3;
    }

    /**
     * Sets the value of the cmeld3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CMELD3CONTENT }
     *     
     */
    public void setCMELD3(CMELD3CONTENT value) {
        this.cmeld3 = value;
    }

    /**
     * Gets the value of the cmeld4 property.
     * 
     * @return
     *     possible object is
     *     {@link CMELD4CONTENT }
     *     
     */
    public CMELD4CONTENT getCMELD4() {
        return cmeld4;
    }

    /**
     * Sets the value of the cmeld4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CMELD4CONTENT }
     *     
     */
    public void setCMELD4(CMELD4CONTENT value) {
        this.cmeld4 = value;
    }

}
