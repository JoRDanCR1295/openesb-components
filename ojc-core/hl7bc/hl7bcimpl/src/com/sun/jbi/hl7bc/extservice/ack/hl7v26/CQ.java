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
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for CQ complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CQ">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}CQ.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CQ.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CQ", propOrder = {
    "cq1",
    "cq2"
})
@XmlSeeAlso({
    TQ1CONTENT.class
})
public class CQ {

    @XmlElement(name = "CQ.1")
    protected CQ1CONTENT cq1;
    @XmlElement(name = "CQ.2")
    protected CQ2CONTENT cq2;

    /**
     * Gets the value of the cq1 property.
     * 
     * @return
     *     possible object is
     *     {@link CQ1CONTENT }
     *     
     */
    public CQ1CONTENT getCQ1() {
        return cq1;
    }

    /**
     * Sets the value of the cq1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CQ1CONTENT }
     *     
     */
    public void setCQ1(CQ1CONTENT value) {
        this.cq1 = value;
    }

    /**
     * Gets the value of the cq2 property.
     * 
     * @return
     *     possible object is
     *     {@link CQ2CONTENT }
     *     
     */
    public CQ2CONTENT getCQ2() {
        return cq2;
    }

    /**
     * Sets the value of the cq2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CQ2CONTENT }
     *     
     */
    public void setCQ2(CQ2CONTENT value) {
        this.cq2 = value;
    }

}
