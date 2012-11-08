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
 * @(#)CWE.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.hl7bc.extservice.ack.hl7v251;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for CWE complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CWE">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}CWE.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CWE.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CWE.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CWE.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CWE.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CWE.6" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CWE.7" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CWE.8" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CWE.9" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CWE", namespace = "urn:hl7-org:v2xml", propOrder = {
    "cwe1",
    "cwe2",
    "cwe3",
    "cwe4",
    "cwe5",
    "cwe6",
    "cwe7",
    "cwe8",
    "cwe9"
})
@XmlSeeAlso({
    ERR10CONTENT.class,
    ERR3CONTENT.class,
    ERR5CONTENT.class,
    ERR11CONTENT.class
})
public class CWE {

    @XmlElement(name = "CWE.1", namespace = "urn:hl7-org:v2xml")
    protected CWE1CONTENT cwe1;
    @XmlElement(name = "CWE.2", namespace = "urn:hl7-org:v2xml")
    protected CWE2CONTENT cwe2;
    @XmlElement(name = "CWE.3", namespace = "urn:hl7-org:v2xml")
    protected CWE3CONTENT cwe3;
    @XmlElement(name = "CWE.4", namespace = "urn:hl7-org:v2xml")
    protected CWE4CONTENT cwe4;
    @XmlElement(name = "CWE.5", namespace = "urn:hl7-org:v2xml")
    protected CWE5CONTENT cwe5;
    @XmlElement(name = "CWE.6", namespace = "urn:hl7-org:v2xml")
    protected CWE6CONTENT cwe6;
    @XmlElement(name = "CWE.7", namespace = "urn:hl7-org:v2xml")
    protected CWE7CONTENT cwe7;
    @XmlElement(name = "CWE.8", namespace = "urn:hl7-org:v2xml")
    protected CWE8CONTENT cwe8;
    @XmlElement(name = "CWE.9", namespace = "urn:hl7-org:v2xml")
    protected CWE9CONTENT cwe9;

    /**
     * Gets the value of the cwe1 property.
     * 
     * @return
     *     possible object is
     *     {@link CWE1CONTENT }
     *     
     */
    public CWE1CONTENT getCWE1() {
        return cwe1;
    }

    /**
     * Sets the value of the cwe1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CWE1CONTENT }
     *     
     */
    public void setCWE1(CWE1CONTENT value) {
        this.cwe1 = value;
    }

    /**
     * Gets the value of the cwe2 property.
     * 
     * @return
     *     possible object is
     *     {@link CWE2CONTENT }
     *     
     */
    public CWE2CONTENT getCWE2() {
        return cwe2;
    }

    /**
     * Sets the value of the cwe2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CWE2CONTENT }
     *     
     */
    public void setCWE2(CWE2CONTENT value) {
        this.cwe2 = value;
    }

    /**
     * Gets the value of the cwe3 property.
     * 
     * @return
     *     possible object is
     *     {@link CWE3CONTENT }
     *     
     */
    public CWE3CONTENT getCWE3() {
        return cwe3;
    }

    /**
     * Sets the value of the cwe3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CWE3CONTENT }
     *     
     */
    public void setCWE3(CWE3CONTENT value) {
        this.cwe3 = value;
    }

    /**
     * Gets the value of the cwe4 property.
     * 
     * @return
     *     possible object is
     *     {@link CWE4CONTENT }
     *     
     */
    public CWE4CONTENT getCWE4() {
        return cwe4;
    }

    /**
     * Sets the value of the cwe4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CWE4CONTENT }
     *     
     */
    public void setCWE4(CWE4CONTENT value) {
        this.cwe4 = value;
    }

    /**
     * Gets the value of the cwe5 property.
     * 
     * @return
     *     possible object is
     *     {@link CWE5CONTENT }
     *     
     */
    public CWE5CONTENT getCWE5() {
        return cwe5;
    }

    /**
     * Sets the value of the cwe5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CWE5CONTENT }
     *     
     */
    public void setCWE5(CWE5CONTENT value) {
        this.cwe5 = value;
    }

    /**
     * Gets the value of the cwe6 property.
     * 
     * @return
     *     possible object is
     *     {@link CWE6CONTENT }
     *     
     */
    public CWE6CONTENT getCWE6() {
        return cwe6;
    }

    /**
     * Sets the value of the cwe6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CWE6CONTENT }
     *     
     */
    public void setCWE6(CWE6CONTENT value) {
        this.cwe6 = value;
    }

    /**
     * Gets the value of the cwe7 property.
     * 
     * @return
     *     possible object is
     *     {@link CWE7CONTENT }
     *     
     */
    public CWE7CONTENT getCWE7() {
        return cwe7;
    }

    /**
     * Sets the value of the cwe7 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CWE7CONTENT }
     *     
     */
    public void setCWE7(CWE7CONTENT value) {
        this.cwe7 = value;
    }

    /**
     * Gets the value of the cwe8 property.
     * 
     * @return
     *     possible object is
     *     {@link CWE8CONTENT }
     *     
     */
    public CWE8CONTENT getCWE8() {
        return cwe8;
    }

    /**
     * Sets the value of the cwe8 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CWE8CONTENT }
     *     
     */
    public void setCWE8(CWE8CONTENT value) {
        this.cwe8 = value;
    }

    /**
     * Gets the value of the cwe9 property.
     * 
     * @return
     *     possible object is
     *     {@link CWE9CONTENT }
     *     
     */
    public CWE9CONTENT getCWE9() {
        return cwe9;
    }

    /**
     * Sets the value of the cwe9 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CWE9CONTENT }
     *     
     */
    public void setCWE9(CWE9CONTENT value) {
        this.cwe9 = value;
    }

}
