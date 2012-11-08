//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2008.12.04 at 12:45:30 AM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v26;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for CCD complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CCD">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}CCD.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CCD.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CCD", propOrder = {
    "ccd1",
    "ccd2"
})
public class CCD {

    @XmlElement(name = "CCD.1")
    protected CCD1CONTENT ccd1;
    @XmlElement(name = "CCD.2")
    protected CCD2CONTENT ccd2;

    /**
     * Gets the value of the ccd1 property.
     * 
     * @return
     *     possible object is
     *     {@link CCD1CONTENT }
     *     
     */
    public CCD1CONTENT getCCD1() {
        return ccd1;
    }

    /**
     * Sets the value of the ccd1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CCD1CONTENT }
     *     
     */
    public void setCCD1(CCD1CONTENT value) {
        this.ccd1 = value;
    }

    /**
     * Gets the value of the ccd2 property.
     * 
     * @return
     *     possible object is
     *     {@link CCD2CONTENT }
     *     
     */
    public CCD2CONTENT getCCD2() {
        return ccd2;
    }

    /**
     * Sets the value of the ccd2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CCD2CONTENT }
     *     
     */
    public void setCCD2(CCD2CONTENT value) {
        this.ccd2 = value;
    }

}