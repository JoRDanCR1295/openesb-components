/*
 * StatsSvc.java
 *
 * Created on March 28, 2007, 1:04 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.soabi.proceng2ejb;

import javax.ejb.Stateless;
import javax.jws.WebService;

/**
 *
 * @author fkieviet
 */

@Stateless()
@WebService()
public class StatsSvc2 {
    public static Stats sStats = new Stats();
    
    public Stats getStats() {
        return sStats.copy();
    }
}
