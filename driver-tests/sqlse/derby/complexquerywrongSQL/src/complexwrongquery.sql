select e.city, e.credit_limit, d.discount_code from "COMPANY"."CUSTOMER" e, "COMPANY"."DISCOUNT_CODE" d where e.zip=d.discount_code