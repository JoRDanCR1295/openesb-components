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
 * @(#)PrivateExtensionMapModel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model;

import java.util.Collection;
import java.util.Set;

/**
 * In memory model of the Private Extension Map that's part of every
 * <code>CLOBRepositoryObjectWithRefs</code>.
 *
 * @author Sun Microsystems
 * @version 
 * @since   5.1.0
 */
public interface PrivateExtensionMapModel {
    
    /** Private Extension GUID prefix */
    public static final String PXUID_PREFIX = "PXUID-";
    
    /** Gets the next UID for a key in the map.
     * @return  Next UID
     */
    String getNextUID();
    
    /** Removes the repository object refererence for specified key.
     * @param   key     Key for repository object refererence.
     */
    void remove(String key);
    
    /** Gets a collection view of the values in this map.
     * @return  Collection of values.
     */
    Collection values();
    
    /** Gets a collection view of the keys in this map.
     * @return  Collection of keys.
     */
    Set keys();
    
    /** Clears all the key/value pairs in this map.
     */
    void clear();
    
    /** Clears all the inUse flags of the map.
     */
    void clearAllInUse();
    
    /** Remove entries that are not in use.
     */
    void removeAllUnused();
    
    /** Finds occurence of a particular <code>RepositoryObjectReference</code> value
     * in the map.
     *
     * @param   val     <code>RepositoryObjectReference</code> value to find.  Any component of
     *                  <code>RepositoryObjectReference</code> that's <code>null</code> is skipped
     *                  in the search.
     * @return  Key to the specified value or <code>null</code> if none found.
     */
    //String findValue(RepositoryObjectReference val);
    
}
