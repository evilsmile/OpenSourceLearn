#-*- coding:utf8
import tensorflow as tf
import numpy as np

# 生成100个点
x_data = np.float32(np.random.rand(2, 100))  # 随机输入 
y_data = np.dot([0.100, 0.200], x_data) + 0.300

# 构造线性模型
b = tf.Variable(tf.zeros([1]))
w = tf.Variable(tf.random_uniform([1,2], -1,0, 1.0))
y = tf.matmul(w, x_data) + b

print x_data
print y_data
print b
print w
print y

# 最小化方差
loss = tf.reduce_mean(tf.square(y - y_data))
optimizer = tf.train.GradientDescentOptimizer(0.5)
train = optimizer.minimize(loss)

# 初始化向量
init = tf.initialize_all_variables()

# 启动图
sess = tf.Session()
sess.run(init)

# 拟合平面
for step in xrange(0, 201):
    sess.run(train)
    if step % 20 == 0:
        print step, sess.run(w), sess.run(b)

