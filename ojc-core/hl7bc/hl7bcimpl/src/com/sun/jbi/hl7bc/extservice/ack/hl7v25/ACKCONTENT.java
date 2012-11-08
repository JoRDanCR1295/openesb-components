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


package com.sun.jbi.hl7bc.extservice.ack.hl7v25;

import java.util.ArrayList;
import java.util.List;
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
 *         &lt;element ref="{urn:hl7-org:v2xml}SFT" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSA"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ACK.CONTENT", propOrder = {
    "msh",
    "sft",
    "msa",
    "err"
})
public class ACKCONTENT {

    @XmlElement(name = "MSH", required = true)
    protected MSHCONTENT msh;
    @XmlElement(name = "SFT")
    protected List<SFTCONTENT> sft;
    @XmlElement(name = "MSA", required = true)
    protected MSACONTENT msa;
    @XmlElement(name = "ERR")
    protected List<ERRCONTENT> err;

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
     * Gets the value of the sft property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the sft property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getSFT().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link SFTCONTENT }
     * 
     * 
     */
    public List<SFTCONTENT> getSFT() {
        if (sft == null) {
            sft = new ArrayList<SFTCONTENT>();
        }
        return this.sft;
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
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the err property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getERR().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ERRCONTENT }
     * 
     * 
     */
    public List<ERRCONTENT> getERR() {
        if (err == null) {
            err = new ArrayList<ERRCONTENT>();
        }
        return this.err;
    }

}
