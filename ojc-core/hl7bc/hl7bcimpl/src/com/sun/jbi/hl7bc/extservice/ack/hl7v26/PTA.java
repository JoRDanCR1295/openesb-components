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
 * <p>Java class for PTA complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PTA">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}PTA.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PTA.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PTA.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PTA.4" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PTA", propOrder = {
    "pta1",
    "pta2",
    "pta3",
    "pta4"
})
public class PTA {

    @XmlElement(name = "PTA.1")
    protected PTA1CONTENT pta1;
    @XmlElement(name = "PTA.2")
    protected PTA2CONTENT pta2;
    @XmlElement(name = "PTA.3")
    protected PTA3CONTENT pta3;
    @XmlElement(name = "PTA.4")
    protected PTA4CONTENT pta4;

    /**
     * Gets the value of the pta1 property.
     * 
     * @return
     *     possible object is
     *     {@link PTA1CONTENT }
     *     
     */
    public PTA1CONTENT getPTA1() {
        return pta1;
    }

    /**
     * Sets the value of the pta1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PTA1CONTENT }
     *     
     */
    public void setPTA1(PTA1CONTENT value) {
        this.pta1 = value;
    }

    /**
     * Gets the value of the pta2 property.
     * 
     * @return
     *     possible object is
     *     {@link PTA2CONTENT }
     *     
     */
    public PTA2CONTENT getPTA2() {
        return pta2;
    }

    /**
     * Sets the value of the pta2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PTA2CONTENT }
     *     
     */
    public void setPTA2(PTA2CONTENT value) {
        this.pta2 = value;
    }

    /**
     * Gets the value of the pta3 property.
     * 
     * @return
     *     possible object is
     *     {@link PTA3CONTENT }
     *     
     */
    public PTA3CONTENT getPTA3() {
        return pta3;
    }

    /**
     * Sets the value of the pta3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PTA3CONTENT }
     *     
     */
    public void setPTA3(PTA3CONTENT value) {
        this.pta3 = value;
    }

    /**
     * Gets the value of the pta4 property.
     * 
     * @return
     *     possible object is
     *     {@link PTA4CONTENT }
     *     
     */
    public PTA4CONTENT getPTA4() {
        return pta4;
    }

    /**
     * Sets the value of the pta4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PTA4CONTENT }
     *     
     */
    public void setPTA4(PTA4CONTENT value) {
        this.pta4 = value;
    }

}
