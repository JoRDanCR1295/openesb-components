//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.04.18 at 09:20:26 PM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v24;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


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

    @XmlElement(name = "MSH", namespace = "urn:hl7-org:v2xml", required = true)
    protected MSHCONTENT msh;
    @XmlElement(name = "MSA", namespace = "urn:hl7-org:v2xml", required = true)
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
