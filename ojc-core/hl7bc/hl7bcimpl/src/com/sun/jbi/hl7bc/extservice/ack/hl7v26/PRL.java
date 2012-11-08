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
 * <p>Java class for PRL complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PRL">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}PRL.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PRL.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PRL.3" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PRL", propOrder = {
    "prl1",
    "prl2",
    "prl3"
})
public class PRL {

    @XmlElement(name = "PRL.1")
    protected PRL1CONTENT prl1;
    @XmlElement(name = "PRL.2")
    protected PRL2CONTENT prl2;
    @XmlElement(name = "PRL.3")
    protected PRL3CONTENT prl3;

    /**
     * Gets the value of the prl1 property.
     * 
     * @return
     *     possible object is
     *     {@link PRL1CONTENT }
     *     
     */
    public PRL1CONTENT getPRL1() {
        return prl1;
    }

    /**
     * Sets the value of the prl1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PRL1CONTENT }
     *     
     */
    public void setPRL1(PRL1CONTENT value) {
        this.prl1 = value;
    }

    /**
     * Gets the value of the prl2 property.
     * 
     * @return
     *     possible object is
     *     {@link PRL2CONTENT }
     *     
     */
    public PRL2CONTENT getPRL2() {
        return prl2;
    }

    /**
     * Sets the value of the prl2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PRL2CONTENT }
     *     
     */
    public void setPRL2(PRL2CONTENT value) {
        this.prl2 = value;
    }

    /**
     * Gets the value of the prl3 property.
     * 
     * @return
     *     possible object is
     *     {@link PRL3CONTENT }
     *     
     */
    public PRL3CONTENT getPRL3() {
        return prl3;
    }

    /**
     * Sets the value of the prl3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PRL3CONTENT }
     *     
     */
    public void setPRL3(PRL3CONTENT value) {
        this.prl3 = value;
    }

}