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
 * @(#)DebuggableEngine.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.netbeans.modules.bpel.debuggerbdi.rmi.api;

/**
 * DebuggableEngine interface that retrieves the variables from bpel
 * engine. When bpel starts a new activity, a new DebuggableEngine is created and passed
 * to Debugger Client, Debugger Client queries the variable through DebuggableEngine
 *
 * @author Sun Microsystems
 * @version 
 */
public interface DebuggableEngine {

    /**
     * Get all available variables
     *
     * @return all available variables in array
     */
    public String[] getVariables();

    /**
     * Returns container data as an xml String
     *
     * @param containerName Container/Variable name
     *
     * @return the container data as String
     */
    public String getContainerDataAsString(String containerName);
    
    /**
     * Returns the variable for given variable name.
     *
     * @param containerName Container/Variable name
     * @return the variable.
     */
    public BPELVariable getVariable(String variableName);
    
    public String[] getPartnerLinks();
    
    public BPELPartnerLink getPartnerLink(String name);
    
    /**
     * Returns the value as String for the xpathExpression
     * @param xpathExpression
     * @return The value as String of the xpathExpression.
     */
    public String evaluate (String xpathExpression) throws XpathExpressionException;

    /**
     * Changes the leaf node value of a WSDL message part, which is a complex type of element for given variable in this callframe. 
     * @param  variableName    the name of the variable
     * @param  partName        the name of the part
     * @param  xpath           the xpath of the leaf node 
     * @param  value           the new value
     * @throws XpathExpressionException   if xpath is invalid.
     * @throws SchemaViolationException if <code>variableName</code> is a XML Schema type variable or <code>xpath</code> indicates
     * non-leaf node.
     */
    public void changeVariableMessageTypeValue(String variableName, String partName, String xpath, String value)
        throws XpathExpressionException, SchemaViolationException;
    
    /**
     * Changes value of a WSDL message part, which is a simple type for given variable in this callframe. 
     * @param  variableName    the name of the variable
     * @param  partName        the name of the part
     * @param  value           the new value
     * @throws SchemaViolationException if <code>variableName</code> is a XML Schema type variable
     */
    public void changeVariableMessageTypeValue(String variableName, String partName, String value)
        throws SchemaViolationException;    
    
    /**
     * Changes value of a XML Schema simple type variable in this callframe. 
     * @param  variableName    the name of the variable
     * @param  value           the new value
     * @throws SchemaViolationException if <code>variableName</code> is a type of WSDL message or <code>xpath</code> indicates 
     * non-leaf node.
     */
    public void changeVariableSchemaTypeValue(String variableName, String value)
         throws SchemaViolationException;
    
    /**
     * Changes the leaf node value of a XML Schema complex type variable in this callframe. 
     * @param  variableName    the name of the variable
     * @param  xpath           the xpath of the leaf node 
     * @param  value           the new value
     * @throws SchemaViolationException if <code>variableName</code> is a type of WSDL message or <code>xpath</code> indicates 
     * non-leaf node.
     */
    public void changeVariableSchemaTypeValue(String variableName, String xpath, String value)
         throws XpathExpressionException, SchemaViolationException;
    
}
