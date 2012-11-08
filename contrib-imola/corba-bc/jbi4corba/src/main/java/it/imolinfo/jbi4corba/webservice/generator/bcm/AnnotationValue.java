 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * XXX javadoc.
 */
public class AnnotationValue {

    public static final int ANNOTATION_VISIT = 0;
    public static final int ANNOTATION_VISIT_ANNOTATION = 1;
    public static final int ANNOTATION_VISIT_ARRAY = 2;
    public static final int ANNOTATION_VISIT_ENUM = 3;

    public int type;
    public String name;
    public String desc;
    public Object value;

    List<AnnotationValue> nestedAnnotationValue = new ArrayList<AnnotationValue>();

    /**
     * @param type  The type
     */
    public AnnotationValue(int type) {
      this.type = type;
    }

    /**
     * @return  The return
     */
    public String toString() {
      return ReflectionToStringBuilder.toString(this);
    }
}
