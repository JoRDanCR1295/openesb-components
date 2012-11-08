/*
 * @(#)BusinessObjectManagerTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.wsdl.schema;

import java.util.HashSet;
import java.util.Set;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class BusinessObjectManagerTest {

    public BusinessObjectManagerTest() {
    }

    @Test
    public void union() {
        Integer k1 = 5;
        Integer k2 = 7;
        Integer k3 = 12;
        
        Integer m1 = 2;
        Integer m2 = 7;
        
        Set<Integer> m = new HashSet<Integer>();
        Set<Integer> k = new HashSet<Integer>();
        
        m.add(m1);
        m.add(m2);
        
        k.add(k1);
        k.add(k2);
        k.add(k3);
        
        Set<Integer> result = BusinessObjectManager.union(m, k);
        
        assertEquals(4, result.size());
    }
}
