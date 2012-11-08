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
 * @(#)NodeSet.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.util.List;

/**
 * NodeSet interface can be used as the type of an argument of an extension
 * function.  Alternatively, the function can declare the argument as
 * a Collection (or List or Set), in which case it will be given a collection
 * of <i>values</i> matching the path.
 * 
 * @author <a href="mailto:dmitri@apache.org">Dmitri Plotnikov</a>
 * @version 
 */
public interface NodeSet {

    /**
     * Returns a list of nodes.
     */
    List getNodes();
    
    /**
     * Returns a list of pointers for all nodes in the set.
     */
    List getPointers();
    
    /**
     * Returns a list of values of all contained pointers.
     */
    List getValues();
    
}
