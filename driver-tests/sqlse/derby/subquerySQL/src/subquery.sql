select * from "COMPANY"."CUSTOMER" where credit_limit in (select MIN (credit_limit) from "COMPANY"."CUSTOMER" group by discount_code)