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

/**
 * Parses the parameter types from a methond internal description. * 
 * 
 * The descriptors of the primitive types are single characters: Z for boolean, C
 * for char, B for byte, S for short, I for int, F for float, J for long and D
 * for double. The descriptor of a class type is the internal name of this class,
 * preceded by L and followed by a semicolon. For instance the type descriptor
 * of String is Ljava/lang/String;. Finally the descriptor of an array type is
 * a square bracket followed by the descriptor of the array element type.
 * Examples are: <br/>
 * <li/> I -> int 
 * <li/> [I ->int[]
 * <li/> Ljava/lang/String; -> String
 * <li/> [Ljava/lang/String -> String[]
 * 
 * A method description contains the parameter list with the return type, for
 * example: <br/>
 * ([Lit/imolinfo/jbi4corba/test/webservice/generator/EchoStruct;[ILorg/omg/CORBA/StringHolder;)Ljava/lang/String;
 * 
 * This first implementation parses only the method paramter form the wholde description.
 * 
 * @author marco
 *
 */
public class InternalMethodDescriptionParser {

    private String description;
    
    private String methodDescriptionTail;
    
    public InternalMethodDescriptionParser(String desc) {
        this.description = desc;
    }
    
    /**
     * Parses the description string, separating the method "tail" (with the return type) and returns the array of 
     * parameters (in internal form).
     * @return
     */
    public List<String> parse() {
        List<String> params = new ArrayList<String>();
        int firstBrPos = description.indexOf('(');
        int lastBrPos = description.indexOf(')');
        String parameterDesc = description.substring(firstBrPos + 1, lastBrPos);
        methodDescriptionTail = description.substring(lastBrPos +1);
        String descToConsume = parameterDesc;
        while (!("".equals(descToConsume))) {
            String nextParameter = getNextParam(descToConsume);   
            params.add(nextParameter);
            // consumes th description
            descToConsume = descToConsume.substring(nextParameter.length(), descToConsume.length());           
        }                        
        return params;
    }
            
    /**
     * Gets the next internal parameter description
     * 
     * @param param
     * 
     * @return the next param
     */
    private String getNextParam(String param) {

        if ((param == null) || (param.equals(""))) {
            return "";
        }

        // Symple types
        if ((param.charAt(0) == 'Z') ||
            (param.charAt(0) == 'C') ||
            (param.charAt(0) == 'B') ||
            (param.charAt(0) == 'S') ||
            (param.charAt(0) == 'I') ||
            (param.charAt(0) == 'F') ||
            (param.charAt(0) == 'J') ||
            (param.charAt(0) == 'D')) {                             
            return new Character(param.charAt(0)).toString();            
        } else if (param.charAt(0) == '[') {
            // Recursive call
            String paramArray = param.charAt(0) + getNextParam(param.substring(1, param.length()));
            return paramArray;
        } else if (param.charAt(0) == 'L') {
            // Object:recursive call
            int pos = param.indexOf(';');
            String paramArray = param.substring(0, pos + 1);
            return paramArray;
        } else {
            return "";
        }
    }

    public String getMethodDescriptionTail() {
        return methodDescriptionTail;
    }

    public void setMethodDescriptionTail(String methodDescriptionTail) {
        this.methodDescriptionTail = methodDescriptionTail;
    }                    
    
    
}
