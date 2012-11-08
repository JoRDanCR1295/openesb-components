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
 * @(#)VID.java 
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
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.VID;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.VID1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.VID2CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.VID3CONTENT;


/**
 * <p>Java class for VID complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="VID">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}VID.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}VID.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}VID.3" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "VID", namespace = "urn:hl7-org:v2xml", propOrder = {
    "vid1",
    "vid2",
    "vid3"
})
public class VID {

    @XmlElement(name = "VID.1", namespace = "urn:hl7-org:v2xml")
    protected VID1CONTENT vid1;
    @XmlElement(name = "VID.2", namespace = "urn:hl7-org:v2xml")
    protected VID2CONTENT vid2;
    @XmlElement(name = "VID.3", namespace = "urn:hl7-org:v2xml")
    protected VID3CONTENT vid3;

    /**
     * Gets the value of the vid1 property.
     * 
     * @return
     *     possible object is
     *     {@link VID1CONTENT }
     *     
     */
    public VID1CONTENT getVID1() {
        return vid1;
    }

    /**
     * Sets the value of the vid1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link VID1CONTENT }
     *     
     */
    public void setVID1(VID1CONTENT value) {
        this.vid1 = value;
    }

    /**
     * Gets the value of the vid2 property.
     * 
     * @return
     *     possible object is
     *     {@link VID2CONTENT }
     *     
     */
    public VID2CONTENT getVID2() {
        return vid2;
    }

    /**
     * Sets the value of the vid2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link VID2CONTENT }
     *     
     */
    public void setVID2(VID2CONTENT value) {
        this.vid2 = value;
    }

    /**
     * Gets the value of the vid3 property.
     * 
     * @return
     *     possible object is
     *     {@link VID3CONTENT }
     *     
     */
    public VID3CONTENT getVID3() {
        return vid3;
    }

    /**
     * Sets the value of the vid3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link VID3CONTENT }
     *     
     */
    public void setVID3(VID3CONTENT value) {
        this.vid3 = value;
    }

}
