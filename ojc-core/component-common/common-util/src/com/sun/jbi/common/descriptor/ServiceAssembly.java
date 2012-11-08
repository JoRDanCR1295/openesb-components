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
 * @(#)ServiceAssembly.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor;

import com.sun.jbi.common.descriptor.model.AssemblyUnit;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.descriptor.model.Identification;

/**
 * Models a service assembly, consisting of a name, contained service units,
 * and configured service connections.
 * 
 * @author Kevan Simpson
 */
public class ServiceAssembly {
    private Identification mId;
    private AssemblyUnit[] mUnits;
    private Connection[] mConnections;
    
    /**
     * Constructs a <code>ServiceAssembly</code>.
     * @param id The assembly's id.
     * @param units The assembly's service units.
     * @param cons The configured connections.
     */
    public ServiceAssembly(Identification id, AssemblyUnit[] units, Connection[] cons) {
        mId = id;
        mUnits = units;
        mConnections = cons;
    }
    
    /**
     * Fetches the assembly's id.
     * @return the assembly's id.
     */
    public Identification getIdentification() {
        return mId;
    }
    
    /**
     * Fetches the assembly's service units.
     * @return the assembly's service units.
     */
    public AssemblyUnit[] getServiceUnits() {
        if (mUnits == null) {
            return new AssemblyUnit[0];
        }
        
        AssemblyUnit[] units = new AssemblyUnit[mUnits.length];
        System.arraycopy(mUnits, 0, units, 0, mUnits.length);
        return units;
    }

    /**
     * Returns the <code>connection</code> entries in the descriptor.
     * @return the <code>connection</code> entries in the descriptor.
     */
    public Connection[] getConnections() {
        // FindBug warning fix - make copy to avoid exposing internal representation
        // Fix to preserve original semantics.  provides object may be null;
        if (mConnections == null) {
            return new Connection[0];
        }

        int len = mConnections.length;
        Connection[] dest = new Connection[len];
        System.arraycopy(mConnections, 0, dest, 0, len);
        return dest;
    }
}
