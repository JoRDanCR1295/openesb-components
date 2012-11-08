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
 * @(#)ConnectionInfoPersister.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */



package com.sun.jbi.mqbc.recovery;

import java.util.Properties;

/**
 *
 * Interface for persister of connection information
 */
public interface ConnectionInfoPersister {
    
    /**
     * Initializes the connection info persister.
     *
     * @throws ConnectionInfoPersistException upon error.
     */
    public void initialize(Properties persisterProps) 
    throws ConnectionInfoPersistException;
    
    /**
     * Persists the connection info records. Merge with any existing ones.
     *
     * @param connInfoRecords Array of connection info records
     * @throws ConnectionInfoPersistException upon error.
     */
    public void persist (ConnectionInfoRecord [] connInfoRecords) 
    throws ConnectionInfoPersistException;

    /**
     * Remove connection info records that match.
     *
     * @param connInfoRecords Array of connection info records
     * @throws ConnectionInfoPersistException upon error.
     */
    public void remove (ConnectionInfoRecord [] connInfoRecords) 
    throws ConnectionInfoPersistException;
    
    
    /**
     * Return all records if any, zero length array otherwise.
     *
     * @throws ConnectionInfoPersistException upon error.
     */
    public ConnectionInfoRecord []  retrieve()
    throws ConnectionInfoPersistException;
    
    /**
     * Closes the persister. Once closed, attempting to reuse persister
     * without initializing again will cause an error.
     *
     * @throws ConnectionInfoPersistException upon error.
     */
    public void close() throws ConnectionInfoPersistException;

}
