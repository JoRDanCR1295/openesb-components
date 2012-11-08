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
 * @(#)ExpressionContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.util.List;

/**
 * If an extenstion function has an argument of type ExpressionContext,
 * it can gain access to the current node of an XPath expression context.
 * <p>
 * Example:
 * <blockquote><pre>
 * public class MyExtenstionFunctions {
 *    public static String objectType(ExpressionContext context){
 *       Object value = context.getContextNodePointer().getValue();
 *       if (value == null){
 *           return "null";
 *       }
 *       return value.getClass().getName();
 *    }
 * }
 * </pre></blockquote>
 *
 * You can then register this extension function using a {@link ClassFunctions
 * ClassFunctions} object and call it like this:
 * <blockquote><pre>
 *   "/descendent-or-self::node()[ns:objectType() = 'java.util.Date']"
 * </pre></blockquote>
 * This expression will find all nodes of the graph that are dates.
 */
public interface ExpressionContext {
    
    /**
     * Get the JXPathContext in which this function is being evaluated.
     *
     * @return A list representing the current context nodes.
     */
    JXPathContext getJXPathContext();

    /**
     * Get the current context node.
     *
     * @return The current context node pointer.
     */
    Pointer getContextNodePointer();

    /**
     * Get the current context node list.  Each element of the list is
     * a Pointer.
     *
     * @return A list representing the current context nodes.
     */
    List getContextNodeList();

    /**
     * Returns the current context position.
     */
    int getPosition();
}
