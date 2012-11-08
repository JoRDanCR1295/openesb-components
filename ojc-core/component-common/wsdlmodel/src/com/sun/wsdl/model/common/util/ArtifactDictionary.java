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
 * @(#)ArtifactDictionary.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.util;

import javax.xml.namespace.QName;

import java.util.Collection;
import java.util.Set;

/**
 * Describes a dictonary of artifacts.
 * @author Sun Microsystems
 * @version 
 */
public interface ArtifactDictionary {
    
    /** Combines the specified artifact dictionary with this.
     * @param   dict    Artifact dictionary to combine with
     */
    void putAll(ArtifactDictionary dict);
    
    /** Puts an artifact into the dictionary.
     * If any of the input params ns, cls, name, artifact is null or empty 
     * dictionary is not populated. 
     * @param   ns          Controlling namespace.
     * @param   cls         Class (type) of artifact.
     * @param   name        Name of artifact.
     * @param   artifact    Artifact to store.
     * @return  Previously stored artifact or <code>null</code> if none.
     */
    Object put(String ns, Class cls, String name, Object artifact);
       
    /** Gets an artifact from the dictionary.
     * @param   ns          Controlling namespace.
     * @param   cls         Class (type) of artifact.
     * @param   name        Name of artifact.
     * @return  Matching artifact or <code>null</code> if not found.
     */
    Object get(String ns, Class cls, String name);
    
    /** Gets all the artifacts contained in the dictionary.
     * @return  Collection of artifacts.
     */
    Collection get();
    
    /** Gets all the artifacts matching a controlling namespace.
     * @param   ns          Controlling namespace.
     * @return  Collection of artifacts.
     */
    Collection get(String ns);
    
    /** Gets all the artifacts matching a controlling namespace and class.
     * @param   ns          Controlling namespace.
     * @param   cls         Class (type) of artifact.
     * @return  Collection of artifacts.
     */
    Collection get(String ns, Class cls);
    
    /** Gets all the artifacts matching a class.
     * @param   cls         Class (type) of artifact.
     * @return  Collection of artifacts.
     */
    Collection get(Class cls);
    
    /** Gets all the artifacts matching a name.
     * @param   name        Name of artifact.
     * @return  Collection of artifacts.
     */
    Collection getByName(String name);
    
    /** Gets all the artifact namespace keys in the dictionary.
     * @return  Set of namespaces.
     */
    Set getNamespaceKeys();
    
    /** Gets all the artifact type (class) keys in the dictionary.
     * @return  Set of types as classes.
     */
    Set getTypeKeys();
    
    /** Gets all the artifact name keys in the dictionary.
     * @return  Set of names.
     */
    Set getNameKeys();
    
    /** Gets all the artifact namespace keys in the dictionary of a certain type.
     * @param   cls     Class (type) of artifact.
     * @return  Set of namespaces.
     */
    Set getNamespaceKeys(Class cls);
    
    /** Gets the size of the artifact dictionary.
     * @return  Number of artifacts in dictionary.
     */
    int size();
    
    /** Tests if the artifact dictionary is empty.
     * @return  <code>true</code> if there are no artifacts.
     */
    boolean isEmpty();
}
