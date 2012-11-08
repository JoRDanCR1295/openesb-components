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
 * @(#)ACKCONTENT.java 
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
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.ACKCONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.ERRCONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSACONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSHCONTENT;


/**
 * <p>Java class for ACK.CONTENT complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ACK.CONTENT">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSH"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSA"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ACK.CONTENT", namespace = "urn:hl7-org:v2xml", propOrder = {
    "msh",
    "msa",
    "err"
})
public class ACKCONTENT {

    @XmlElement(name = "MSH", namespace = "urn:hl7-org:v2xml")
    protected MSHCONTENT msh;
    @XmlElement(name = "MSA", namespace = "urn:hl7-org:v2xml")
    protected MSACONTENT msa;
    @XmlElement(name = "ERR", namespace = "urn:hl7-org:v2xml")
    protected ERRCONTENT err;

    /**
     * Gets the value of the msh property.
     * 
     * @return
     *     possible object is
     *     {@link MSHCONTENT }
     *     
     */
    public MSHCONTENT getMSH() {
        return msh;
    }

    /**
     * Sets the value of the msh property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSHCONTENT }
     *     
     */
    public void setMSH(MSHCONTENT value) {
        this.msh = value;
    }

    /**
     * Gets the value of the msa property.
     * 
     * @return
     *     possible object is
     *     {@link MSACONTENT }
     *     
     */
    public MSACONTENT getMSA() {
        return msa;
    }

    /**
     * Sets the value of the msa property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSACONTENT }
     *     
     */
    public void setMSA(MSACONTENT value) {
        this.msa = value;
    }

    /**
     * Gets the value of the err property.
     * 
     * @return
     *     possible object is
     *     {@link ERRCONTENT }
     *     
     */
    public ERRCONTENT getERR() {
        return err;
    }

    /**
     * Sets the value of the err property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERRCONTENT }
     *     
     */
    public void setERR(ERRCONTENT value) {
        this.err = value;
    }

}
