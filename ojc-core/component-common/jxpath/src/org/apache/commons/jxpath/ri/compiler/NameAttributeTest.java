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
 * @(#)NameAttributeTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;


/**
 * Captures the <code>foo[@name=<i>expr</i>]</code> expression. These
 * expressions are handled in a special way when applied to beans
 * or maps.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class NameAttributeTest extends CoreOperationEqual {

    public NameAttributeTest(Expression namePath, Expression nameValue) {
        super(namePath, nameValue);
    }

    public Expression getNameTestExpression() {
        return args[1];
    }

    /**
     * @see Expression#computeContextDependent()
     */
    public boolean computeContextDependent() {
        return true;
    }
}
