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
 * @(#)SwareDOMImplementation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware;

import javax.xml.transform.Transformer;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;

import com.sun.xml.transform.sware.schema.SwareSchema;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;

/**
 * The interface provides services in creating XML schema aware DOM documents
 * and in helping reordering of the elements in an existing DOM document under
 * the guidance of the governing schema.  The DOM documents created by the
 * interface behave same as normal DOM documents except all DOM nodes will
 * be Proxy instances and their
 * <code>appendChild</code> method is intercepted to take care of the correct
 * ordering of the elements based on the governing schema.  So basically
 * after the <code>appendChild</code> method call returns, the last child may
 * not be the node that just gets appended.
 * 
 * <p>Neither a schema aware DOM document nor an element reordering transformer
 * guarantees the most desired document being generated after the population
 * process is done.  In certain situations, finding a most desired place
 * to insert an element or reordering elements to the most desired order is
 * considered impossible.  Considering following schema:
 * 
 * <p>&lt;?xml version=&quot;1.0&quot; encoding=&quot;UTF-8&quot;?&gt;
 * <p>&lt;xs:schema xmlns:xs=&quot;http://www.w3.org/2001/XMLSchema&quot;
 * <p>         elementFormDefault=&quot;qualified&quot; attributeFormDefault=&quot;unqualified&quot;&gt;
 * <p>    &lt;xs:element name=&quot;A&quot;&gt;
 * <p>        &lt;xs:complexType&gt;
 * <p>            &lt;xs:sequence&gt;
 * <p>                &lt;xs:element name=&quot;B&quot; maxOccurs=&quot;unbounded&quot;/&gt;
 * <p>                &lt;xs:sequence&gt;
 * <p>                    &lt;xs:element name=&quot;C&quot;/&gt;
 * <p>                    &lt;xs:element name=&quot;B&quot; maxOccurs=&quot;unbounded&quot;/&gt;
 * <p>                &lt;/xs:sequence&gt;
 * <p>            &lt;/xs:sequence&gt;
 * <p>        &lt;/xs:complexType&gt;
 * <p>    &lt;/xs:element&gt;
 * <p>&lt;/xs:schema&gt;
 * <p>
 * <p>
 * when <code>A.addB()</code> is called if C already exists, the process does
 * not know where to put <code>B</code> (it can be put either before
 * <code>C</code> or after <code>C</code>).  When this kind of situation does
 * occur, some policies need to be used to determine what to do.
 *  
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public interface SwareDOMImplementation {

    /**
     * Creates a new schema aware DOM Document.
     * 
     * @param namespaceURI the namespace URI of the document element
     *             (might be null)
     * @param qualifiedName the qualified name of the document element
     *             (must not be null)
     * @param schema the XML schema (an instance of SwareSchema), where the
     *               definition of the document element can be found (must
     *               not be null)
     * @param policies the policies used in the processing when abnormal
     *                 condition occurs (might be null)
     * @return a schema aware DOM document
     * @throws DOMException DOM exception
     */
    public Document createDocument(String namespaceURI,
            String qualifiedName, SwareSchema swareSchema, Policies policies)
        throws DOMException;
    
    /**
     * Creates an element reordering transformer based on an XML schema.
     * The transformer can be used to reordering the elements in an existing
     * DOM document.
     *  
     * @param schema an XML schema (an instance of SwareSchema, must not be null)
     * @param policies the policies used in the processing when abnormal
     *                 condition occurs (might be null)
     * @throws InvalidSchemaException invalid schema exception
     * @return an element reordering transformer
     */
    public Transformer createReorderTransformer(
            SwareSchema swareSchema, Policies policies)
        throws InvalidSchemaException;

    /**
     * Creates a new schema aware DOM Document.
     * 
     * @param namespaceURI the namespace URI of the document element
     *             (might be null)
     * @param qualifiedName the qualified name of the document element
     *             (must not be null)
     * @param swareTypeSystem the XML type system (an instance of SwareTypeSystem),
     *               where the definition of the document element can be found
     *               (must not be null)
     * @param policies the policies used in the processing when abnormal
     *                 condition occurs (might be null)
     * @return a schema aware DOM document
     * @throws DOMException DOM exception
     */
    public Document createDocument(String namespaceURI,
            String qualifiedName, SwareTypeSystem swareTypeSystem,
            Policies policies)
        throws DOMException;
    
    /**
     * Creates an element reordering transformer based on an XML type system.
     * The transformer can be used to reordering the elements in an existing
     * DOM document.
     *  
     * @param swareTypeSystem an XML type system (an instance of
     *          SwareTypeSystem, must not be null)
     * @param policies the policies used in the processing when abnormal
     *                 condition occurs (might be null)
     * @throws InvalidSchemaException invalid schema exception
     * @return an element reordering transformer
     */
    public Transformer createReorderTransformer(
            SwareTypeSystem swareTypeSystem, Policies policies)
        throws InvalidSchemaException;
    
    /**
     * An instance of this class can be used to pass a set of policies to the
     * element ordering/reordering process.
     */
    public class Policies {
        
        /**
         * Array that holds the policies
         */
        private Object[] mPolicies = new Object[Byte.MAX_VALUE + 1];
        
        /**
         * Sets a policy.
         * 
         * @param policyID the policy ID
         * @param policy the policy value
         */
        public void setPolicy(byte policyID, byte policy) {
            if (policyID < 0) {
                throw new ArrayIndexOutOfBoundsException(
                        "Policy ID must be greater than or equal to 0: "
                        + policyID);
            }
            mPolicies[policyID] = new Byte(policy);
        }
        
        /**
         * Sets a policy.
         * 
         * @param policyID the policy ID
         * @param policy the policy value
         */
        public void setPolicy(byte policyID, Object policy) {
            if (policyID < 0) {
                throw new ArrayIndexOutOfBoundsException(
                        "Policy ID must be greater than or equal to 0: "
                        + policyID);
            }
            mPolicies[policyID] = policy;
        }

        /**
         * Tests if a policy does exist.
         * 
         * @param policyID policy ID
         * @return true if the policy does exist, otherwise false
         */
        public boolean hasPolicy(byte policyID) {
            if (policyID < 0) {
                return false;
            }
            return mPolicies[policyID] != null;
        }

        /**
         * Gets the byte type policy value.
         * 
         * @param policyID policy ID
         * @return byte type policy value
         */
        public byte getPolicyByte(byte policyID) {
            if (policyID < 0) {
                throw new ArrayIndexOutOfBoundsException(
                        "Policy ID must be greater than or equal to 0: "
                        + policyID);
            }
            if (mPolicies[policyID] == null
                    || !(mPolicies[policyID] instanceof Byte)) {
                return Byte.MIN_VALUE;
            }
            return ((Byte) mPolicies[policyID]).byteValue();
        }
        
        /**
         * Gets the Object type policy value.
         * 
         * @param policyID policy ID
         * @return Object type policy value
         */
        public Object getPolicyObject(byte policyID) {
            if (policyID < 0) {
                throw new ArrayIndexOutOfBoundsException(
                        "Policy ID must be greater than or equal to 0: "
                        + policyID);
            }
            return mPolicies[policyID];
        }
    }
    
    /**
     * This abstract class defines the constants for the ordering fail over
     * policy.
     */
    public abstract class FOPolicy {
        /**
         * The policy ID
         */
        public static final byte ID = 1;

        /**
         * Option of keeping the original order (default)
         */
        public static final byte KEEP_ORIGINAL_ORDER = 0;
        
        /**
         * Option of always throwing exception
         */
        public static final byte THROW_EXCEPTION = 1;
    }
    
    public abstract class Parameters {
        /**
         * Parameter for passing in XML type to the transformer.  Value must
         * be of <code>javax.xml.naming.QName</code> type.
         */
        public static final String XML_TYPE = "xmltype";
        
        /**
         * Parameter passed into the transformer to indicate if the sorting
         * should be done in-place or in a new tree. Value must be of
         * <code>java.lang.Boolean</code> type. Default is <code>true</code>.
         */
        public static final String IN_PLACE = "inplace";
        
        /**
         * Parameter passed into the transformer to indicate if the order
         * should be checked before sorting.  So if the elements in the DOM
         * tree is already in correct, no sorting is needed. Value must be of
         * <code>java.lang.Boolean</code> type. Default is <code>true</code>.
         */
        public static final String PRE_CHECKING = "prechecking";
    }
}
