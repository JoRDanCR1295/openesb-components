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
 * @(#)ObjectRegistry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.util;

import java.util.*;

/**
 * The <em>ObjectRegistry</em> is used to do name-to-object reference lookups.
 * If an <em>ObjectRegistry</em> is passed as a constructor argument, then this
 * <em>ObjectRegistry</em> will be a cascading registry: when a lookup is
 * invoked, it will first look in its own table for a name, and if it's not
 * there, it will cascade to the parent <em>ObjectRegistry</em>.
 * All registration is always local. [??]
 * 
 * @author   Sanjiva Weerawarana
 * @author   Matthew J. Duftler
 */
public class ObjectRegistry {
  Hashtable      reg    = new Hashtable ();
  ObjectRegistry parent = null;

  public ObjectRegistry () {
  }
  
  public ObjectRegistry (Map initialValues) {
    if(initialValues != null)
    {
      Iterator itr = initialValues.keySet().iterator();
      while(itr.hasNext())
      {
        String name = (String) itr.next();
        register(name, initialValues.get(name));
      }
    }
  }

  public ObjectRegistry (ObjectRegistry parent) {
    this.parent = parent;
  }

  // register an object
  public void register (String name, Object obj) {
    reg.put (name, obj);
  }

  // unregister an object (silent if unknown name)
  public void unregister (String name) {
    reg.remove (name);
  }

  // lookup an object: cascade up if needed
  public Object lookup (String name) throws IllegalArgumentException {
    Object obj = reg.get (name);

    if (obj == null && parent != null) {
      obj = parent.lookup (name);
    }

    if (obj == null) {
      throw new IllegalArgumentException ("object '" + name + "' not in registry");
    }

    return obj;
  }
}
