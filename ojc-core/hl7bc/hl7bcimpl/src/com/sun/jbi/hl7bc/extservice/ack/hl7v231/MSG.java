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
 * @(#)MSG.java 
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
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSG;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSG1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSG2CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSG3CONTENT;


/**
 * <p>Java class for MSG complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MSG">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSG.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSG.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSG.3" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MSG", namespace = "urn:hl7-org:v2xml", propOrder = {
    "msg1",
    "msg2",
    "msg3"
})
public class MSG {

    @XmlElement(name = "MSG.1", namespace = "urn:hl7-org:v2xml")
    protected MSG1CONTENT msg1;
    @XmlElement(name = "MSG.2", namespace = "urn:hl7-org:v2xml")
    protected MSG2CONTENT msg2;
    @XmlElement(name = "MSG.3", namespace = "urn:hl7-org:v2xml")
    protected MSG3CONTENT msg3;

    /**
     * Gets the value of the msg1 property.
     * 
     * @return
     *     possible object is
     *     {@link MSG1CONTENT }
     *     
     */
    public MSG1CONTENT getMSG1() {
        return msg1;
    }

    /**
     * Sets the value of the msg1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSG1CONTENT }
     *     
     */
    public void setMSG1(MSG1CONTENT value) {
        this.msg1 = value;
    }

    /**
     * Gets the value of the msg2 property.
     * 
     * @return
     *     possible object is
     *     {@link MSG2CONTENT }
     *     
     */
    public MSG2CONTENT getMSG2() {
        return msg2;
    }

    /**
     * Sets the value of the msg2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSG2CONTENT }
     *     
     */
    public void setMSG2(MSG2CONTENT value) {
        this.msg2 = value;
    }

    /**
     * Gets the value of the msg3 property.
     * 
     * @return
     *     possible object is
     *     {@link MSG3CONTENT }
     *     
     */
    public MSG3CONTENT getMSG3() {
        return msg3;
    }

    /**
     * Sets the value of the msg3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSG3CONTENT }
     *     
     */
    public void setMSG3(MSG3CONTENT value) {
        this.msg3 = value;
    }

}
