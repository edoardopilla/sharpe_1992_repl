# sharpe_1992_repl

**_Replicating results from the paper by William Sharpe, published in 1992 in the Journal of Portfolio Management._**

In his paper, Sharpe provides an analysis of several mutual funds in order to extract each own's "style", namely by arguing that investing in them will lead to a risk and reward profile comparable to investing in a certain amount of the underlying risk factors, such as value or growth factors. He then states that a simple linear regression of the fund's return on the various factors is not suitable, since mutual funds often have a short selling constraint which prevents them from taking up short positions across asset classes, hence he subsequently formulates a quadratic programming problem which allows him to obtain an indication of the tracking error variance.

This replication attempt uses monthly data from a set of German mutual funds, along with monthly data about a set of indexes which track different asset classes, to carry out the style analysis consistently with Sharpe.

Weights allocated for each asset class are plotted on rolling windows, along with the respective window's R squared, again consistently with Sharpe.
