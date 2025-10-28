import * as React from "react";
import { BlogCard } from "./BlogCard";

const blogPosts = [
  {
    image:
      "https://api.builder.io/api/v1/image/assets/TEMP/57260afea11eb7a91ddd2a0f01102321b10ea6bf?placeholderIfAbsent=true&apiKey=c00da45f4ed64d9893425a2094d3e1af",
    category: "Knowledge graph",
    title: "Connected ideas",
    description: "Different ideas connect to enhance the understanding",
    imageAspectRatio: "aspect-[1.4]",
  },
  {
    image:
      "https://api.builder.io/api/v1/image/assets/TEMP/ac6bf10a50171593e7fc18ecfd5350f5fe1f488e?placeholderIfAbsent=true&apiKey=c00da45f4ed64d9893425a2094d3e1af",
    category: "Topics",
    title: "Topics cloud",
    description:
      "Linear helps streamline software projects, sprints, tasks, and bug tracking. Here's how to get started.",
    imageAspectRatio: "aspect-[1.4]",
  },
  {
    image:
      "https://api.builder.io/api/v1/image/assets/TEMP/80f805ac20c26d656e695eee1cc18445d3eb7170?placeholderIfAbsent=true&apiKey=c00da45f4ed64d9893425a2094d3e1af",
    category: "Structured Content",
    title: "Maps of contents",
    description:
      "Organized maps for not getting lost in the sea of exploration",
    imageAspectRatio: "aspect-[1.53]",
  },
];

export function BlogGrid() {
  return (
    <section className="px-8 mt-36 w-full max-w-screen-xl max-md:px-5 max-md:mt-10 max-md:max-w-full">
      <div className="w-full max-md:max-w-full">
        <div className="flex flex-wrap gap-8 justify-center items-start w-full max-md:max-w-full">
          {blogPosts.map((post, index) => (
            <BlogCard
              key={index}
              image={post.image}
              category={post.category}
              title={post.title}
              description={post.description}
              imageAspectRatio={post.imageAspectRatio}
            />
          ))}
        </div>
      </div>
    </section>
  );
}
