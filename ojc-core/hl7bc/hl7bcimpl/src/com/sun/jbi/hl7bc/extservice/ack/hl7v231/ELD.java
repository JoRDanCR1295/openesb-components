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
 * @(#)ELD.java 
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


package com.sun.jbi.hl7bc.extservice.ack.hl7v231;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.ELD;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.ELD1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.ELD2CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.ELD3CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.ELD4CONTENT;


/**
 * <p>Java class for ELD complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ELD">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}ELD.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ELD.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ELD.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ELD.4" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ELD", namespace = "urn:hl7-org:v2xml", propOrder = {
    "eld1",
    "eld2",
    "eld3",
    "eld4"
})
public class ELD {

    @XmlElement(name = "ELD.1", namespace = "urn:hl7-org:v2xml")
    protected ELD1CONTENT eld1;
    @XmlElement(name = "ELD.2", namespace = "urn:hl7-org:v2xml")
    protected ELD2CONTENT eld2;
    @XmlElement(name = "ELD.3", namespace = "urn:hl7-org:v2xml")
    protected ELD3CONTENT eld3;
    @XmlElement(name = "ELD.4", namespace = "urn:hl7-org:v2xml")
    protected ELD4CONTENT eld4;

    /**
     * Gets the value of the eld1 property.
     * 
     * @return
     *     possible object is
     *     {@link ELD1CONTENT }
     *     
     */
    public ELD1CONTENT getELD1() {
        return eld1;
    }

    /**
     * Sets the value of the eld1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ELD1CONTENT }
     *     
     */
    public void setELD1(ELD1CONTENT value) {
        this.eld1 = value;
    }

    /**
     * Gets the value of the eld2 property.
     * 
     * @return
     *     possible object is
     *     {@link ELD2CONTENT }
     *     
     */
    public ELD2CONTENT getELD2() {
        return eld2;
    }

    /**
     * Sets the value of the eld2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ELD2CONTENT }
     *     
     */
    public void setELD2(ELD2CONTENT value) {
        this.eld2 = value;
    }

    /**
     * Gets the value of the eld3 property.
     * 
     * @return
     *     possible object is
     *     {@link ELD3CONTENT }
     *     
     */
    public ELD3CONTENT getELD3() {
        return eld3;
    }

    /**
     * Sets the value of the eld3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ELD3CONTENT }
     *     
     */
    public void setELD3(ELD3CONTENT value) {
        this.eld3 = value;
    }

    /**
     * Gets the value of the eld4 property.
     * 
     * @return
     *     possible object is
     *     {@link ELD4CONTENT }
     *     
     */
    public ELD4CONTENT getELD4() {
        return eld4;
    }

    /**
     * Sets the value of the eld4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ELD4CONTENT }
     *     
     */
    public void setELD4(ELD4CONTENT value) {
        this.eld4 = value;
    }

}
