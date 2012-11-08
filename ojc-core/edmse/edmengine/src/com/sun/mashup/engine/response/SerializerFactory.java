/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.mashup.engine.response;

/**
 *
 * @author admin
 */
public class SerializerFactory {
    
    public static Serializer getInstance(String type) {
        Serializer instance = null;
        if( type.equalsIgnoreCase(ResponseTypes.JSON) ) {
            instance = new JSONSerializer();
        } else if (type.equalsIgnoreCase(ResponseTypes.RELATIONALMAP)) {
            instance = new XMLSerializer();
        } else if (type.equalsIgnoreCase(ResponseTypes.WEBROWSET)) {
            instance = new WebRowSetSerializer();
        }
        return instance;
    }
    
}
