/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: DaoConfig.java,v 1.1.1.1 2007/04/09 17:49:27 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.errordb;

import java.io.Reader;

import com.bostechcorp.cbesb.common.util.RuntimeClassLoader;
import com.ibatis.common.resources.Resources;
import com.ibatis.sqlmap.client.SqlMapClient;
import com.ibatis.sqlmap.client.SqlMapClientBuilder;

public class DaoConfig {

  private static final String resource = "errordb-map-config.xml";
  private  SqlMapClient sqlMap;

  
  public SqlMapClient getSqlMapInstance() {
    try {
     /* Properties props = Resources.getResourceAsProperties("database.properties");
      String url = props.getProperty("url");
      String driver = props.getProperty("driver");
      String username = props.getProperty("username");
      String password = props.getProperty("password");
      if (url.equals("jdbc:derby:jbiDB")) {
        Class.forName(driver).newInstance();
    
      }*/
      Resources.setDefaultClassLoader(RuntimeClassLoader.getClassLoader());
      Reader reader = Resources.getResourceAsReader(resource);
//      System.out.println(Resources.getResourceAsFile(resource).getAbsolutePath());
      sqlMap= SqlMapClientBuilder.buildSqlMapClient(reader);
     
    } catch (Exception e) {
      throw new RuntimeException("Description.  Cause: " + e, e);
    }
    return sqlMap;
  }

 

}
