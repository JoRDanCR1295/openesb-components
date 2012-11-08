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
 * @(#)PojoSEEndpoitn.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi;

import com.sun.jbi.common.descriptor.EndpointInfo;

/**
 * TODO 
 * @author Girish Patil
 */
public class PojoSEEndpoint {
    private EndpointInfo ep;
    
    /**
     * @param info
     */
    public PojoSEEndpoint(EndpointInfo info) {
        ep = info; 
    }

    @Override
    public boolean equals(Object obj) {
        boolean ret = false;
        if (obj instanceof PojoSEEndpoint){
            PojoSEEndpoint other = (PojoSEEndpoint)obj;
            if (this.ep == null) {
                if (other.ep == null){
                    ret = true;
                }
            }else {
                if ((this.ep != null) && (other.ep != null)){
                    ret = this.ep.equals(other.ep);
                }
            }
        }
        // else other obj is null, 'this' is not!
        return ret;
    }

    @Override
    public int hashCode() {
        return this.hashCode() + ((this.ep != null) ? this.ep.hashCode(): 0);
    }
    
    
}
