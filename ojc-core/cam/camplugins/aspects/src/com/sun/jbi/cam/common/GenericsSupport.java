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
 * @(#)GenericsSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.common;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @author graj
 *
 */
public class GenericsSupport {

	/**
	 * 
	 */
	public GenericsSupport() {
		// TODO Auto-generated constructor stub
	}
	
    /**
     * We can now use this to produce lists of ints or Strings:
     * List<Integer> ints = Lists.toList(1, 2, 3);
     * List<String> names = Lists.toList("Gopalan", "Suresh", "Raj");
     * 
     * @param array
     * @return
     */
    public static <Type> List<Type> toList(Type... array) {
        List<Type> list = new ArrayList<Type>();
        for (Type arrayElement : array) {
        	list.add(arrayElement);
        }
        return list;
    }
    
    /**
     * 
     * @param collection
     * @param componentType
     * @return
     */
    @SuppressWarnings("unchecked")
    static public <Type> Type[] toArray(Collection<Type> collection,
            Class<Type> componentType) {
        // unchecked cast
        Type[] array = (Type[]) java.lang.reflect.Array.newInstance(componentType,
                collection.size());
        int index = 0;
        for (Type value : collection) {
            array[index++] = value;
        }
        return array;
    }
	

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
