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
 * @(#)XMLAttribute.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model;

/**
 * Describes a XML element attribute.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface XMLAttribute extends XMLNode {
    
    /** Enumerated values for an optional boolean type */
    public static final String[] BOOLEAN_ENUM_VALS = {"yes", "no"};
    
    /** Prefix for SeeBeyond private extension namespace: sbynpx */
    public static final String SBYNPX = "sbynpx";
    
    /** Prefix with colon for SeeBeyond private extension namespace: sbynpx: */
    public static final String SBYNPX_COLON = SBYNPX + ":";
    
    /** Namespace for SeeBeyond private extension */
    public static final String SBYNPX_NAMESPACE = "http://bpel.seebeyond.com/hawaii/5.0/privateExtension/";

    /**Namespace for SeeBeyond runtime*/
    public static final String SEEBEYOND_RUNTIME_NAMESPACE = SBYNPX_NAMESPACE + "runtime/";
    
    /** Prefix for SeeBeyond private extension presentation namespace: sbynpxp */
    public static final String SBYNPXP = SBYNPX + "p";
    
    /** Prefix with colon for SeeBeyond private extension presentation namespace: sbynpxp: */
    public static final String SBYNPXP_COLON = SBYNPXP + ":";
    
    /** Namespace for SeeBeyond private extension presentation */
    public static final String SBYNPXP_NAMESPACE = SBYNPX_NAMESPACE + "presentation/";
    

    /** Prefix for SeeBeyond private extension persistence namespace: sbynpers */
    public static final String SBYNPERS = "sbynpers";

    /** Prefix with colon for SeeBeyond private extension persistence namespace: sbynpers: */
    public static final String SBYNPERS_COLON = SBYNPERS + ":";

    /** Namespace for SeeBeyond private extension persistence*/
    public static final String SBYNPERS_NAMESPACE 
        = SBYNPX_NAMESPACE + "persistence/";

    /**Prefix for SeeBeyond engine namespaces: sbynRunTime */
    public static final String SBYN_RUNTIME = "sbynruntime";

    /** Prefix with colon for SeeBeyond engine namespace: sbynRunTime: */
    public static final String SBYN_RUNTIME_COLON = SBYN_RUNTIME + ":";
    
    /** Getter for property optional.
     * @return Value of property optional.
     *
     */
    boolean isOptional();
    
    /** Setter for property optional.
     * @param optional New value of property optional.
     *
     */
    void setOptional(boolean optional);
    
    /** Getter for property enumValues.
     * @return Value of property enumValues.
     *
     */
    String[] getEnumValues();
    
    /** Setter for property enumValues.
     * @param enumValues New value of property enumValues.
     *
     */
    void setEnumValues(String[] enumValues);
    
    /** Determines if a value is within the enumerated values.
     * @param   v   Value to be tested.
     * @return  <tt>true</tt> if the value is within the set.
     */
    boolean isInEnumValues(String v);
    
    /** Getter for property type.
     * @return Value of property type.
     *
     */
    Class getType();
    
    /** Setter for property type.
     * @param type New value of property type.
     *
     */
    void setType(Class type);
    
}
