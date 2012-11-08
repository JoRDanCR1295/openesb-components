/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.mashup.engine.response;

import java.sql.ResultSet;

/**
 *
 * @author admin
 */
public interface Serializer {
    public String serialize(ResultSet rs,String column,int offset) throws Exception;
}
