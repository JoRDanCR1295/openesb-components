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
 * @(#)SwareGroup.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema;

/**
 * Wrapping interface for model group definition.  More methods might
 * be added if more information is needed from the wrapper.
 *  
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public interface SwareGroup extends SwareParticle {

    /**
     * Possible compositor types of a model group
     */
    public enum Compositor {
        SEQUENCE,
        CHOICE,
        ALL
    }

    /**
     * Gets compositor of the model group.
     * 
     * @return compositor of the model group
     */
    public Compositor getCompositor();
    
    /**
     * Gets all particles (non-recursive).
     * 
     * @return an array of particles
     * @throws SwareSchemaException invalid schema exception
     */
    public SwareParticle[] getParticles() throws SwareSchemaException;
    
    /**
     * Gets the number of inmmediate child particles.
     * 
     * @return the number of inmmediate child particles
     */
    public int getParticleCount() throws SwareSchemaException;
    
    /**
     * Gets the specified particle.
     * 
     * @return an array of particles
     * @throws SwareSchemaException invalid schema exception
     */
    public SwareParticle getParticle(int i) throws SwareSchemaException;
    
    /**
     * Tests if the model group is a reference.
     * 
     * @return <code>true</code> if it is a reference, otherwise <code>false<code>
     */
    public boolean isReference();
    
    /**
     * Gets the referenced model group if this group is a reference.
     * 
     * @return the referenced model group, or <code>null</code> if this group
     *         is not a reference.
     */
    public SwareGroup getReference();
}
